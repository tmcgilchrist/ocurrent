## Lets Build a CI

Everyone loves to build a new CI system! It's one of life's great joys to ignore anyone
that has gone before and re-invent the wheel. What better way to do it than in production
with a system integral to shipping code like CI.

Being a sensible person you keep your code in `git`, afterall we aren't complete savages,
and you host it on a public GitLab instance. See https://gitlab.com.

The OCaml library [ocurrent][] provides us with the building blocks to represent build graphs
as incremental computations, so no need to shave that yak. What it is missing is a plugin
to force GitLab events into the computation graph.

Create a file `plugins/gitlab/current_gitlab.ml` for our new plugin.

### Pre-Requisites


This guide will use OCaml, opam and dune. It is assumed you have those installed and have
some familiarity with the OCaml programming language. Consult your OS documentation for installing
opam. After that create a switch to work in `opam switch create 4.12.1-gitlab 4.12.1` and when that's done building the 4.12.1 version of the OCaml compiler use that switch `opam switch use 4.12.1-gitlab`.

Clone the ocurrent repo as
`git clone git@github.com/ocurrent/ocurrent.git`

and `cd ocurrent`.

### Part 1 GitLab APIs

GitLab exposes 2 APIs that we need to setup our CI system. First some way of finding what to build,
in GitLab terminology changes are proposed via [Merge Requests][]. Merge Requests contain a series of
git commits on a branch, along with a description of the change being proposed and various pieces of metadata about the Merge Request. The piece we need is the branch and git commits, so we can do
a git checkout of those when the time comes.

The second piece we need is recording the results of a build, this is done against a particular
[Commit][]. We can record the build status as Passed, Failed or Running, along with the results
of that build. If another commit is pushed to the branch for a Merge Request, we can start a new
build for that.

The final piece required is GitLab allows us to register a URL to receive events about a project.
Webhooks enable you to send notifications to web applications in response to events in a group
or project. For our purposes we want to be notified of new commits, branches and merge requests.
Here;s how the default workflow works:

1. Whenever someone pushes code to the project, GitLab sends the `Push event` with data about the
   changes including the git sha.
2. When your application receives this eveent, it can add a [commit status][] to that commit.
3. Then as the build progresses, you update that commit with the results of the build along with
   a short desription and a URL to visit for more information.

The same process occurs for `Merge Request` events, which are created when a Merge Request is
created, updated or new commits are added to it. Indeed we want both events since we want to
run our CI against both the main branch and all Merge Requests.

In this guide, you'll learn how to:

### Step 1.1 Setup on GitLab permissions

Using a project on GitLab, or creating a new project, we need to setup GitLab to deliver the
webhooks we are interested in. For my project page to register webhooks, visit:
https://gitlab.com/tmcgilchrist/freer/-/hooks

Before creating an entry there, we need a tool called Smee to capture webhook payloads and forward them to your local development environment. First, go to https://smee.io and click Start a new channel. Starting a new Smee channel creates a unique domain where GitLab can send webhook payloads.
You'll need to know this domain for the next step. Eg `https://smee.io/P8Dqe8Q0slzkh12345`

Next, go back to the Terminal and install the Smee CLI client:

1. Install the client as
`npm install --global smee-client`

2. Run the client to forward webhooks locally, replacing https://smee.io with your unique URL.

`smee -u https://smee.io/P8Dqe8Q0slzkhfO9 -p 8080 -P "/webhooks/gitlab"`

3. Then create a new Webhook using your unique URL, and Check the rriggers for
 * Merge request events
 * Push events
 For now don't worry about adding a secret token, we will come back to that.

### Step 1.2 Adding event handling

Now that we are getting webhooks delivered by GitLab we need to start processing them.
GitLab sends webhook payloads as `POST` requests, using Smee those webhooks will be receieved
as `POST` request payloads at `/webhooks/gitlab`. The code looks like:

``` ocaml
let webhook = object
    inherit Current_web.Resource.t

    method! post_raw _site req body =
      let headers = Cohttp.Request.headers req in
      let event = Cohttp.Header.get headers "X-Gitlab-Event" in
      Log.info (fun f ->
        f "Got GitLab event %a" Fmt.(option ~none:(any "NONE") (quote string)) event);
      Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"OK" ()
```

We provide a function that creates an Object, inheriting from `Current_web.Resource.t` some
default methods and behaviour. Then we override a particular method `post_raw` to provide
our handling of GitLab webhooks. For now we get some information from the HTTP headers, on
what event is being delivered, log that and return HTTP 200 back to GitLab.

Then we register this handler under the `/webhooks/gitlab` url as

``` ocaml
  let routes =
    Routes.(s "webhooks" / s "gitlab" /? nil @--> Gitlab.webhook) ::
    Current_web.routes engine
```

### Step 1.3 Creating a Commit status

Creating a commit status is handled by `ocaml-gitlab` library, which provides an api to set the
commit status.

``` ocaml
val status : token:Token.t ->
             project_id:int ->
             sha:string ->
             state:Gitlab_t.commit_status_status ->
             name:string ->
             unit -> Gitlab_t.commit_status Response.t Monad.t
```

Here we supply the labelled arguments for `project_id`, `sha`, `state` and a `name` to distinguish
this status from others recorded against this commit. Here we want to set something like `ocaml-ci`.

This shows up in the GitLab UI as a Green, Red or Yellow icon against the commit.

## Part 2: Build on the CI Pipeline
### Step 2.1 Creating an application

Show `doc/examples/gitlab.ml`






Extras
----------

Register External Status Checks to simulate manual approval for merge requests.
https://about.gitlab.com/blog/2021/10/04/how-to-status-checks/

https://docs.github.com/en/developers/apps/guides/creating-ci-tests-with-the-checks-api

https://docs.github.com/en/developers/apps/getting-started-with-apps/setting-up-your-development-environment-to-create-a-github-app#step-1-start-a-new-smee-channel

[Merge Requests]: https://docs.gitlab.com/ee/api/merge_requests.html
[Commit]: https://docs.gitlab.com/ee/api/commits.html
[commit status]: https://docs.gitlab.com/ee/api/commits.html#post-the-build-status-to-a-commit
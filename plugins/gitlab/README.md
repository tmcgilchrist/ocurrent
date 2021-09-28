Lets Build a CI
==========

Everyone loves to build a new CI system! It's one of life's great joys to ignore anyone
that has gone before and re-invent the wheel. What better way to do it than in production
with a system integral to shipping code like CI.

Being a sensible person you keep your code in `git`, afterall we aren't complete savages,
and you host it on a public GitLab instance. See https://gitlab.com.

The OCaml library ocurrent provides us with the building blocks to represent build graphs
as incremental computations, so no need to shave that yak. What it is missing is a plugin
to force GitLab events into the computation graph.

Create a file `plugins/gitlab/current_gitlab.ml` for our new plugin.

Pre-Requisites
----------

This guide will use OCaml, opam and dune. It is assumed you have those installed and have
some familiarity with the OCaml programming language. Consult your OS documentation for installing
opam.

Part 1 Merge Requests API
----------

Decribe the GitLab API for merge requests and recording CI results.


Step 1.1 Setup on GitLab permissions
----------
Using my own repository you need to setup GitLab to deliver webhooks

Project page to register webhooks
https://gitlab.com/tmcgilchrist/freer/-/hooks

Using smee.io to forward webhooks locally.

`smee -u https://smee.io/P8Dqe8Q0slzkhfO9 -p 8080 -P "/webhooks/gitlab"`

Check the Triggers for:
 * Merge request events

Step 1.2 Adding event handling
----------

Using LWT, yojson and CoHTTP.

Step 1.3
----------


Extras
----------

Register External Status Checks to simulate manual approval for merge requests.
https://about.gitlab.com/blog/2021/10/04/how-to-status-checks/
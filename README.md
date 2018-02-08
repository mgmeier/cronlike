## What is Cronlike?

Cronlike is a utility library for automatically running actions based on some schedule.
Its aim is to cover all use cases where you'd want a cron system inside your application, or where you run
multiple cleanup / janitor / garbage collection jobs at specific intervals

# Design

Cronlike is designed:
* to be compact, both in size and complexity
* to be straightforward to use
* not to bloat project dependencies
* not to incur any penalty wrt. performance and/or memory usage
* to support registering and unregistering cronjobs during runtime
* to have an unintrusive API
* and to be general enough for all targeted use cases

# API

TBD.

* data types

* `cronlikeRegister` - destructive insert/update of a job
* `cronlikeUnrgister` - remove (classes of) jobs
* `cronlikeList` - list currently registered jobs

# Example usage

TBD - see ["here"](https://github.com/mgmeier/cronlike/blob/master/example/Main.hs)


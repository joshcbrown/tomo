# tomo

tomo is a terminal UI pomodoro timer and task manager for macOS.

## features

- pomdoro timer
- task management
- desktop notifications for cycle changes
- basic stats on usage info accompanied by a github contribution graph style graphic

## usage

below is a video that showcases most of the functionality as of 30/8/25:

<https://github.com/user-attachments/assets/9f74d1fc-8f44-4568-8ea6-bbda1b00e70e>

as shown, there's a help page within the app that you can access with '?'.
for completeness, i've added it below.

```
Welcome! Getting around in tomo is done nearly entirely with the keyboard.
Key binds are context sensitive. There are broadly three contexts to be aware of:

When the timer is focused (as when the app is first opened):
[q]uit the app
[p]ause the timer
[n]ext: skip the current timer
[t]asks: show/hide the task area
[s]tats: show/hide the stats area
[c]omplete task currently being worked on
[i]nsert a task
[j]/[k]: focus tasks area

When tasks are focused:
"[j]: move selection down"
"[k]: move selection up"
"[Esc]: move focus to pomo"
"[Enter]: work on selected task"
"[d]elete the selected task"
"[c]omplete selected task"
"[e]dit selected task"

"When editing a task:"
"[Enter]: add task to task list"
"[Esc]: abort"
```

## why?

tomo is the successor to my previous, very similar project [pogodoro](https://github.com/joshcbrown/pogodoro).

there were a few decisions i made in that project that i disagree with now and am addressing
in this project:

### simplification of ui and workflow

pogodoro has multiple pages. pogodoro has multiple tables, each separated
by a different pre-defined category of task.

the idea for the app is simple. at its core, what i want from it is the ability to:

- work according to a pomodoro schedule;
- add/create discrete and minimal tasks that guide the work; and
- some view of progress.

that's it. i don't think the previous design was well motivated (graphic design is unfortunately
_not_ my passion). using [pomofocus](https://pomofocus.io/) helped me understand and see this.
the current design is largely inspired by it.

### removal of sqlite and sqlx as dependencies

in pogodoro, i chose to bring in sqlite data store for tasks and logs.
this wasn't motivated by a technological need,
but rather a desire to learn the technology (which i still think is fine!).

in the same way (finding cool shiny technology and wanting to learn), i used
sqlx as a client to interact with the database.
the knock-on effects of these decisions were:

- cornering me into using async, which ushered in the whole red/blue function problem, and ballooned the complexity
  of an app that would never need to call into the database more than once every few seconds at most; and
- complicating the build and installation process. because sqlx queries are checked to be type-safe at compile time,
  the user needs to setup the database before the app can be built. this required the installation of sqlx-cli and
  the manual creation of a folder (also why tf did i choose ~/.config as the place for this?)

in other words, the issue that i have now was not my motivation to use these technologies,
but rather my use-case for them.

i've moved to a very simple model of just storing the stupidly-simple serialisation
of the data that i use for tasks and logs in this project in the form of JSON.
it works totally adequately.

one thing that may/may not have been present in the original project is the absence of
any form of concurrent data store access protection. the app currently does the naive thing of
disallowing more than one instance to be open at a given time, which i think is a fine compromise
for the time being.

### moving to a higher-level library

this is the least egregious of the issues i'm addressing with this project.

Rust and [ratatui](https://docs.rs/ratatui/latest/ratatui/widgets/index.html) make for
reasonably ergonomic environments to develop TUIs in. still, it's easy
to see that the marginal performance gains don't justify the
constraints this tech stack imposes. namely:

- having to worry about memory management and the borrow checker
- rolling my own render/event handler pipeline

enter [brick](https://github.com/jtdaugherty/brick), a very high-level
library for building TUIs in haskell. the dev-ex here has been really pleasant
and i think more suitable for the use case. it's also afforded me the opportunity
to dip my toes into the world of lenses, which has been really fun
(and well-motivated this time to boot!).

## contributions

contributions are welcomed and encouraged.

# kfcron

This utility will add tasks to your KanbanFlow board on a schedule. It
automatically picks up changes to the schedule file, no need to restart
process.

## Usage
    $ kfcron [path-to-schedule] [path-to-token]

## Sample Schedule
```yaml
# sample-schedule.yaml
tasks:
- name: Laundry
  column: Prioritized
  category: Self Care
  schedule: '0 0 * * 2,5'

- name: Vocabulary Practice
  column: Prioritized
  category: Graduate School
  schedule: '0 0 * * *'
```

## Full Documentaion
[kfcron-docs repo](https://github.com/larioj/kfcron-docs)

## Build
    $ stack build
    $ stack install

## Development
    : tabe docs/dev.md

### Set up docs
    $ git submodule add https://github.com/larioj/kfcron-docs.git docs

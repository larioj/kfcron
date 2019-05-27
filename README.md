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

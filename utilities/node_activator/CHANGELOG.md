# Changelog for 0.2.0

## 1. Enhancements

* Add `NodeActivator.epmd_running?/0`

## 2. Bug fixes

* Rename one of test code.
* Fix the issue of fully-qualified hostname on Linux. It requires to use IP address instead of hostname.

## 3. Soft deprecations (no warnings emitted)

## 4. Hard deprecations

* Deprecate supporting Windows, temporally. To recover it, need to fix the issue that Node does not respond on Windows (related to #187).


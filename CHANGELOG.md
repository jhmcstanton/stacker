# Changelog

All notable changes to this project will be documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.4.0]
### Added
- More docs
- Dockerized
- Supports stacking for others
- Added support for moving position in stack
- Added links to polciies in footer
### Fix
- Added client-side form validation to disallow blank room or user names
- Handles duplicate usernames

## [0.3.0] - 2021.05.14
### Added
- More sensible stack update when creating new local stack from broad stack
- Added missing cookie deletion functionality

## [0.2.1] - 2021-05-14
### Fix
- Delete user also clears removes the user from local and global stacks

## [0.2.0] - 2021-05-14
### Added
- Better support for clients rejoining rooms (properly refreshes / retrieves room data)
- Support for attendees leaving rooms
- Support for closing rooms

##  [0.1.0] - 2021-05-13
### Added
- Initial implementation of `stacker`. Includes:
-- Basic functionality to support 2 simultaneous stacks
-- Supports single clients/browsers using multiple different rooms
-- Easy sharing

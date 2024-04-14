# [0.0.4.0-beta1] - 2024-04-30

**Warning/Disclaimer:** This update introduces **breaking changes**. Please make sure to back up your world before
updating to this version.
These changes are necessary to enable the new features and to make the mod scalable for future updates and keep it
maintainable. Note that I'll try to keep these changes to a minimum in the future. But this will happen from time to
time to enable new features and to improve the mod.

**Breaking changes relate to:**

- **the way region data is stored** and thus the way it is read/written.

- **how the flags are handled** - in particular, the way flags are inherited and overridden by parent regions

- **the structure and naming of some commands** - which have changed to be more consistent

## Added

* Add commands to copy region properties
* Add enhanced flag management and messages
* Add flag inheritance and overriding for regions
* Add new config options
* Add the Global Region. It's the parent region of all Dimensional Regions. **One region to rule them all!**
* API: New events for flag checks. You can now listen to flag checks and cancel them if needed and listen for the
  result of a check and manipulate the outcome.

### Copy region properties
* Add new command to copy properties from one Local Region to another Local Region:
  * `/wp dim <dim> <region> copy flags to-local <target-dim> <target-region>`: copy all flags from `region`
    to `target-region`
  * `/wp dim <dim> <region> copy flags to-dim <target-dim>`: copy all flags from `region` to `target-dim`
  * `/wp dim <dim> <region> copy state to-local <target-dim> <target-region>`: copy the region state from `region` and
    apply it to `target-region`
  * `/wp dim <dim> <region> copy state to-dim <target-dim>`: copy the region state from `region` and apply it
    to `target-dim`
  * `/wp dim <dim> <region> copy players to-local <target-dim> <target-region> [group] `: copy all players from `region`
    to `target-region`. To copy only a specific group (members, owners) add it as optional parameter
  * `/wp dim <dim> <region> copy players to-dim <target-dim> [group] `: copy all players from `region` to `target-dim`.
    To copy only a specific group (members, owners) add it as optional parameter

* Add new commands to copy properties from one Dimensional Region to another Dimensional Region
  * `/wp dim <dim> copy flags to-local <target-dim> <target-region>`: copy all flags from `dim` to `target-region`
  * `/wp dim <dim> copy flags to-dim <target-dim>`: copy all flags from `dim` to `target-dim`
  * `/wp dim <dim> copy state to-local <target-dim> <target-region>`: copy the region state from `dim` and apply it
    to `target-region`
  * `/wp dim <dim> copy state to-dim <target-dim>`: copy the region state from `dim` and apply it to `target-dim`
  * `/wp dim <dim> copy players to-local <target-dim> <target-region> [group] `: copy all players from `dim`
    to `target-region`. To copy only a specific group (members, owners) add it as optional parameter
  * `/wp dim <dim> copy players to-dim <target-dim> [group] `: copy all players from `dim` to `target-dim`. To copy only
    a specific group (members, owners) add it as optional parameter

### Enhanced Flags
* Flags now have their own dedicated flag message which is shown when the flag is triggered.
* It's now possible to mute flag messages for each individual flag (it is still possible to mute all flags for the
  region).
* Flag messages can contain placeholders for:
  * `{player}` - name of player
  * `{flag}` - name of triggered flag
  * `{region}` - name of involved region
  * `{dimension}` - name of dimension
  * `{pos}` - position of flag activation source \[X=x, Y=y, Z=z\]
* Flag messages also can be formatted by using the minecraft default string formatting.
  * For example `&c{player}&r tried to break a block in &9{region}&r!` will result in a red player name and a blue
    region name.
  * Take a look at [this tool](https://codepen.io/0biwan/pen/ggVemP) for reference as well as
    the [minecraft wiki](https://minecraft.wiki/w/Formatting_codes).

* Flags now have a *FlagState* instead of just being present/absent. When you add a flag, it will have the denied state
  to keep the same behavior as before. The different flag states are described as follows:
  * *Allowed* - The flag is allowed for the region and will be checked.
  * *Denied* - The flag is denied for the region and will be checked.
  * *Disabled* - The flag is disabled for the region and will not be checked.
  * *Undefined* - The flag is not defined for the region.
* Flags can be disabled to keep the flags in the region but disable the flag check. This is useful when you need to
  disable a flag but don't want to lose the flag settings.
* Add commands for enhanced flag management:
  * `/wp flag local <dim> <local> <flag> state <ALLOWED|DENIED|DISABLED>` - set the state for a flag
  * `/wp flag local <dim> <local> <flag> override <true|false>` - sets the flag to override the same flag in child
    regions
  * `/wp flag local <dim> <local> <flag> msg set <msg>` - set a new message for the flag. Check the wiki for a
    description of possible placeholders for messages.
  * `/wp flag local <dim> <local> <flag> msg clear` ...
  * `/wp flag local <dim> <local> <flag> msg mute` ...
  * `/wp flag dim ...` to manage flag properties for a Dimensional Region
  * `/wp flag global ...` to manage flag properties for the Global Region

### Flag inheritance & overriding for regions

* Child regions now inherit the flags of their parent regions. This means that every region will also inherit the flags
  of the corresponding Dimensional region and the Global Region.
* Parent regions can now override flags of child regions to enforce flags onto them.
* Flag pagination now includes parent flags (in italic)
* Flag pagination now also sorts flags by region and flag state
  * WHITE - allowed flags
  * RED - denied flags
  * GRAY - disabled flags
  * DARK GRAY - undefined flags
* Flag pagination shows overriding flags in a bolt/underline font
* Add interactive CLI support for enhanced flag management
  * TODO: FlagState CLI update

### Config
* `yawp-common.toml` - Add new permission config:
  * `allow_region_tp` - Decides whether teleporting inside/outside a region is allowed for everyone. Useful when using
    Waystones in regions for example.
  * `disable_cmd_for_non_op`. Defines whether mod commands are disabled for non-OP players. This is useful when you want
    to use the mod only for OPs or players which have their UUID entry in the config. Enable this if you don't want the
    YAWP command to be seen by non-permitted users.
* `yawp-flags.toml` - Add new flag configs:
  * `enable_flag_inheritance`: Toggles the inheritance of flags of parent regions.
  * `remove_entities_for_spawning_flags`: Toggles the de-spawning of entities when using the `spawning-*` flags.
  * TODO: `break_flag_entities` and `break_flag_entity_tags` are now also used for the `place-blocks` flag.

### Global Region

* The Global Region has the same properties as the Dimensional Regions but is not limited to one dimension. It is the
  parent region of all Dimensional Regions and active everywhere.
* Add new commands for management of the Global Region:
  * `/wp global info`.
  * `/wp global clear flags|players|teams|group`.
  * `/wp global add player|team|flag`.
  * `/wp global remove player|team|flag`.
  * `/wp global list flag|group|dim`.
  * `/wp global state alert|enable`.
* Add interactive CLI support for the Global Region

### API

* Add FlagCheckEvent. This event is fired before a flag is checked. It can be canceled and in the progress can be used
  to cancel the flag check entirely.
* Add FlagCheckResult as an event. This event is fired after a flag check. It can be used to manipulate the
  outcome of the flag check. This event cannot be canceled.

### Misc

* Add new command to expand the area of a Local Region:
  * `/wp local <dim> <local> area expand [yMin] [yMax]`.
  * The optional parameters can be used to set a specific height.
  * Omitting the parameters will set the region area to the Minecraft version specific build limits
* Add new command to rename a Local Region: `/wp local <dim> <local> rename <newName>`.
* Add new commands to add and remove offline players from/to regions.
  * `/wp local <dim> <local> add player <group> by-name <player names separated by space>`.
  * `/wp local <dim> <local> remove player <group> by-name <player names separated by space>`.
  * `/wp local <dim> <local> add player <group> by-uuid <player uuid>`.
  * `/wp local <dim> <local> remove player <group> by-uuid <player uuid>`.
  * Note that you can define multiple names for adding and removing but only one UUID at a time.
  * Same goes for the Global and Dimensional Regions as well
* Dimensional Regions now can be muted (as well as their flags, all or individually)

## Changed

* YAWP can now be used in single-player (open to LAN) worlds. It is still a server-side only mod, but now also works on
  integrated servers / LAN worlds.
* Change flag pagination for regions to include a link to the flag info as well as some quick links.
* Change Dimensional Region CLI to include support for muting regions.
* Flags now have an internal category. This info will also be added to the wiki. The categories are a change to allow
  some other features to be added in the future. The categories are not final and may still change.
* Rename spatial properties to area properties. This change also involves some commands and language keys.
* Change command to update Local Region area:
  * Old: `/wp region <dim> <region> area Cuboid <pos1> <pos2>`
  * New: `/wp local <dim> <local> area set Cuboid <pos1> <pos2>`
* Improved RegionMarker indicators for marked blocks. It's item name now shows colored indicators for
  * the amount of blocks which needs to be marked for a valid area
  * a selected teleport position
* The RegionMarker now also prompts feedback for marked blocks and a valid area to the player.
* Rename affiliation to groups. This change also involves some commands and language keys.
* Change commands to manage Local Regions:
  * Old: `/wp region <dim> <region> ...`
  * New: `/wp local <dim> <local> ...`
* Change commands to manage groups (former known as affiliations):
  * Old: `/wp region <dim> <region> add|list|remove affiliate player|team <member|owner> ...`
  * New: `/wp local <dim> <local> add|list|remove group player|team <member|owner> ...`
* The spawning flags no longer remove entities with the PersistanceRequired tag or a custom name.
* Renaming a Stick to create a RegionMarker is now disabled to prevent permission issues. This will come back in a
  future update with an overhaul of the RegionMarker.
* Moved the region name examples (used when creating a new region) to the language file to enable I18n support for the
  examples.
* Moved the flag message examples to the language file to enable I18n support for the examples.
* Flag list links for regions now also show number of inherited flags in parentheses

## Fixed

* Fix some cli typos
* Fix parent - child hierarchy inconsistency when deleting children

# [0.0.3.0-beta1] - 2024-03-27

## Added

* Add first basic draft for an API to manage
  regions [Pull Request #105](https://github.com/Z0rdak/Yet-Another-World-Protector/pull/105). Thank you very much!
* Implement CreateRegion event which is fired whenever a region is created. This event can be canceled to prevent the
  creation.
* Implement UpdateRegion event which is fired whenever a region is update (the area changed). This event can be
  canceled.
* Implement DeleteRegion event which is fired whenever a region is deleted. This event can be canceled to prevent the
  deletion.

## Changed
* Bump forge version to latest (36.2.42)

# [0.0.2.9-beta3] - 2023-08-12
## Fixed
* [Forge] Fix place-blocks and no-walker-freeze flags not working together properly
* [Fabric] Fix spawning-all flag destroying thrown out items
* [Fabric] Fix inventory desync when placing a block with the `place-blocks` flag active
* Fix command block execution not working
* Fix misleading error message when supplying invalid local region name
* Fix single block region area size being calculated incorrectly
* Fix warn message for invalid keys in yawp-flags.toml config file
* Fix typos
# [0.0.2.9-beta2] - 2023-06-11
## Added
* [1.19.4+] Add default english fallback translation
# [0.0.2.9-beta1] - 2023-06-09
## Added
* [Fabric] Add `mob-griefing`, `enderman-griefing` and `zombie-destruction` flag. Ported by petersv5 aka. ptefar - thanks a lot!
## Changed
* Change default value for config `command_op_level` back to 4. This caused many players to struggle with setting up the
  mod in the first place.
  With the default 4 we assume that the player setting up the server is OP lvl 4 and thus admin.
## Fixed
* [Forge] Replace mixin for `walker-freeze` flag with forge event. This should solve #85.
# [0.0.2.8-beta2] - 2023-04-21
## Fixes
* Fix rare NPE occurring because of random event handler order.
* Fix region command permission not working.
* Fix region owners not be considered for command permission when having `allow_info_cmds` set to `false`.
# [0.0.2.8-beta1] - 2023-04-14
## Added
* Add `leaf-decay` flag (#66).
* Add `fire-tick` flag (#66).
* Add `walker-freeze` flag (#66).
* YAWP now detects whether Journey Map is loaded alongside it and sets the base command to `/yawp`.
## Fixed
* Fix YAWP commands being unregistered upon using `/reload`. This fixes the issue with using Ice & Fire together with YAWP (#75).
* Fix YAWP resource pack not loading correctly at client side.
* Fix `ignite-explosives` causing the server to crash.
# [0.0.2.7-beta2] - 2023-04-09
## Fixed
* Fix YAWP flags not working due to configuration errors (damn mixins!)
# [0.0.2.7-beta1] - 2023-03-29
From now on this changelog will use the keep a changelog format: https://keepachangelog.com/en/1.0.0/. YAWP is still following the schematic versioning suggested by forge: https://docs.minecraftforge.net/en/1.19.x/gettingstarted/versioning/.
## Added
* CLI: Region state quick-link & command enhancements: `/wp region <dim> <region> state enable|alert` and `/wp dim <dim> enable` now can be used without providing the new state (you can still provide the state explicitly though). They work as a toggle used this way. The related quick-links for Dimensional and Local Regions have been adjusted accordingly.
* CLI: Undo links. Most command confirmation messages will now provide a link to undo the action. (#63)
* Spawning-related flags now despawns all entities in the Local Region, which are covered by the flag.
* Spawning-related flags now despawns all entities in the Dimensional Region, which are covered by the flag (exluding entities in the Local Regions without the flag).
## Changed

* CLI: All remove commands will now run the command immediately instead of suggesting it. (#63)
* Breaking change: Unified flag naming. Replaced all `_` with `-`. Invalid flags will be removed automatically from your
  regions. (#64)
  Make sure to re-add the removed flags. Affected flags:
  - `break_blocks` -> `break-blocks`
  - `place_blocks` -> `place-blocks`
  - `place_fluids` -> `place-fluids`
  - `scoop_fluids` -> `scoop-fluids`
## Removed
* Flag `entity-place`. It is replaced by the `enderman-griefing` flag, which covers picking and placing of blocks by endermen.
## Fixed
* Fix Cuboid areas not saving marked blocks correctly. This does not affect already defined areas.
# 0.0.2.6-beta4
## Additions
* Add logging indicator for reloading YAWP config.
* Add logging for invalid default flags for Dimensional and Local Regions provided in `yawp-region-defaults.toml`
* Add logging for loaded default flags for Dimensional and Local Regions provided in `yawp-region-defaults.toml`
* Add logging for loaded UUIDs for `players_with_permission`
* Add logging for loaded block entity identifiers for `break_flag_entities`
* Add logging for loaded block entity tag identifiers for `break_flag_entity_tags`
* Conclusion: Added a bunch of logging to make your and my life easier.
## Fixes
* Fix region flag list pagination not working
* Fix creating a region with marker not prompting the correct parent regions
* Fix creating a region with marker not checking for valid area
* Fix Dimensional Regions not displaying correct amount of Local Regions
# 0.0.2.5-beta4
## Changes
* `command_op_lvl` now can be set to 5 to disable the usage of mod commands for any OP
* `invincible` flag now additionally grants knock back protection
## Fixes
* Fix `invincible` flag not working correctly from changes in 0.0.2.3-beta4
# 0.0.2.4-beta4
## Changes
* Adjust mod logging representation
* Flag `break_blocks` now also covers entities (includes the `break_entities` flag).
## Removals
* Remove flag `break_entities`. This is now covered with `break_blocks` flag. The config stays the same.
## Fixes
* Fix message for missing region permissions not displaying correctly
* Attempt to fix NPE for mob grieving flag
# 0.0.2.3-beta4
## Changes
* `no-pvp` and `knockback-players` flag now denies actions also for owners/members of the region to prevent mischief
## Fixes
* Fix `invincible` and `no-pvp` flag not working correctly
# 0.0.2.2-beta4
## Changes
* Add default entry `minecraft:leash_knot` to break-entities config
* New created regions without owner are no longer disabled by default
## Fixes
* Fix dimension region caches not initialized before traveling to dimension
* Fix some other misc stuff
# 0.0.2.1-beta4 - Region overlapping hotfix
## Fixes
* Fix overlapping regions not correctly considering priority and region member/ownership.
  From now on, regions with the highest priority always should be the deciding region regarding flags.
* Fix Local Regions not overwriting their parent region flags (either Dimensional or Local Regions).
  Local Regions show now disable flags from their parent regions, if they don't define the flags themselves.
  The following table show how flags are handled for region hierarchies:
  |Parent | Child | Result |
  |-------------|-----------------|--------|
  |flag set | flag set | deny |
  |flag not set | flag set | deny |
  |flag set | flag not set | allow |
  |flag set | no child region | deny |
* Fix removing teams from regions not working properly
# 0.0.2.0-beta4 - CLI pagination & new flags
## Additions
* Add flag `use-elytra`. This flag prevents players from using the elytra for flying.
  Specifically, it prevents players from starting. It does not force players to land when they are flying in a zone with
  this flag.
* Add flag `no-flight`. This flag prevents players from flying in a region. It also forces players to fall.
* Add flag `enter-dim` for Dimensional Regions. This flag prevents players from traveling (using a portal or teleporting) to the dimension which has this flag active.
* Add flag `mob-griefing` to prevent mob griefing.
* Add flag `enderman-griefing` to prevent enderman from picking up and placing blocks. This replaces the now removed flag `entity-place`.
* Add pagination support for flags, regions/children, players and teams for the CLI
* Add config option for amount of entries (flags, regions, players or teams) per page: `cli_entries_per_page`. It's located in the `yawp-region-defaults.toml`. It defaults to 5 entries per page.
* Reintroduced the config option to choose the mods root command: `wp_root_command`. It's located in the `yawp-common.toml` config. You are able to choose between `/wp` and `/yawp`, with `/wp` being the default.
## Changes
* Replace most player names in the CLI with player info links
* Replace most team names in the CLI with team info links
* Replace most region names in the CLI with region info links
* Unify affiliate cli for dimensional and local regions
* Unify colors for different actions in the cli
## Fixes
* Fix RegionMarker displaying language keys instead of correct tooltip.
* Fix player inventory not synced correctly after creation region marker by renaming a stick in an anvil.
* Fix player teams not considered for permission checks to manage regions
* Fix `/wp region <dim> <region> area ...` update not checking for parent regions permissions
# 0.0.1.4-beta4 - CLI improvements & priority fixes
## Additions
* Add flag `use-entities` to prevent general entity interactions
* Add flag `use-items` to prevent general item usage
* Add flag `use-blocks` to prevent general block usage/interaction.
  This should, beside other things, prevent using modded containers in contradiction to the access-container flag,
  which only covers containers implementing vanilla mechanics.
* Add some sanity checks when setting region priority, so prevent mistakes for overlapping regions and region hierarchy
## Changes
* Reworked header format (the lines with == ... ==) to be more consistent and add a self-link to each header for easier accessibility.
* Remove `use` flag.
## Fixes
* Fix priority not set properly for Local Regions
* Fix some I18n typos and missing translations
## Notes
* Currently child regions do not overwrite flags of their parents. This means for example when having a parent with the break-block flag, which denies breaking blocks, and a child which doesn't have this flag, the flag of the parent still covers the child region.
# 0.0.1.3-beta4
## Fixes
* Fix mod commands being executable without permission when using new alternative base command yawp
# 0.0.1.2-beta4 - I18n and flag fixes
## Additions
* Add link to Local and Dimensional Region info header to copy region NBT data to clipboard for more easy troubleshooting
* Added several new lang keys to cover the whole interactive CLI
* Added russian translation
* Add `no-pvp` flag. Opposite to the `melee-players` flag, this flag should cover all pvp dmg
* Explosion related flags now work in Local Regions
## Changes
* updated english and german translations
* cleared `break_blocks` and `place_blocks` from the default flags list for new dimensional and new local regions
* `/wp marker give` only works for the executor of the command (if it is a player)
* Remove non-functional `w_command_alt` config. `/yawp` is not a default alternative for `/wp`
## Fixes
* Fix `invincible` flag not working properly
# 0.0.1.1-beta4
## Fixes
* Fix `spawning-villager` flag working for wandering traders
# 0.0.1.0-beta4 - Local Region overhaul
This update brings the first iteration of working Local Regions. Due to some drastic changes in saving and loading region data, older regions are not supported - sorry!
Rest assured, once the mod goes from beta to release, these kinds of changes regarding will be kept to a minimum to ensure backwards compatibility.
## Additions
* Local Region flags are now implemented! Please visit the wiki for a full list of flags and for more information about how the Region hierarchy works.
* Add RegionMarker stick. Use it to mark an area for a new region and then either use the CLI to create the region or rename the stick in an anvil.
* Add sub-command to give a player a RegionMarker: `/wp marker give [<player>]`.
* Add sub-command to reset the state of a RegionMarker the player is holding: `/wp marker reset`.
* Add flag for knock-back protection against players.
  To learn more about the priorities for overlapping regions and region hierarchy please visit the wiki.
* Add flag `spawning-slime`, `spawning-villager`, `spawning-trader`, `drop-loot-player`. Visit the wiki for detailed information about the flags.
## Changes
* Spherical Regions are for now disabled. They will come back later with other shapes.
* When removing a child region from a Local Region, its new parent will be the Dimensional Region and the child will be disabled.
* The flag command `/wp flag ...` is disabled for now. It has currently no real functionality and will come back in the next update.
* Deleting regions no longer works for regions with children. There will be a config option in a future update to enable this, though.
* Rename `attack-*` flags to `melee-*`. They only protect against melee attacks, hence the change.
* Disable explosion related flags. They will come back in the next update.
## Fixes
* Various CLI interface fixes regarding visualization and usability
# 0.0.1.0-beta3 - Minor fixes
## Additions
* Implemented missing command for removing regions
* Add temporary hint explaining not working local region flags and region hierarchy
* Add config option to set default de/activate state for dimensional regions upon creation
## Fixes
* Fix dimensional flags not loading correctly causing them to not work in 1.18 and 1.19.
* Fix new dimensional region created by argument not containing default flags from config.
* Fix dimensional region info not showing local region info properly
# 0.0.1.0-beta2 - Local region preview
## Additions
* Add first beta implementation of local regions.
  They come with two different area types: Cuboid and Sphere. More types will come soon!
  NOTE: The flag checks for local regions are still missing, but you are still able to set up your regions.
  They will be valid and working as soon as the checks are implemented in the next version.
* Add CLI to manage local regions. You are able to manage all properties of local regions with commands and with the
  interactive CLI.
  For a comprehensive command overview take a look at the wiki.
* Add new config `allow_info_cmds` to enable the usage of informative region commands for all players.
* Add new config `dim_default_flags`, replacing `default_flags` for default flags for new dimensional regions.
## Changes
* Command to de-/activate dimensional regions changed from `activate` to `enable`.
* Config option `default_flags` is now used to define default flags for local regions.
## Fixes
* Some minor bug and typo fixes
# 0.0.1.0-beta1 - First beta release
This is the first beta release of this mod. Please be so kind and report any bugs you encounter on the GitHub issue page: https://github.com/Z0rdak/Yet-Another-World-Protector/issues.
If you have suggestions, feel free to post them, too.
* Add online wiki as a guide on how to use the mod: https://github.com/Z0rdak/Yet-Another-World-Protector/wiki
* Add dimensional regions
* Add configuration for default region flags, CLI permission,
* Add many flags to protect your server (visit the wiki for more info)
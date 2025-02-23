# [0.5.2-beta4] - 2025-02-23

## Changed

* Disabled compat with [Snow! Real Magic!](<https://www.curseforge.com/minecraft/mc-mods/snow-real-magic>) to prevent crashes.

# [0.5.2-beta3] - 2025-02-22

## Added

* Add simplified chinese localization. Thank you very much kaixue#6395!

## Fixed

* (1.21.1+) Fix crash when adding player by-uuid or by-name, thanks legenden#7526! ([PR#155](https://github.com/Z0rdak/Yet-Another-World-Protector/pull/155))
* (1.20.1, 1.21.1) Crash due to prev added compat with [Snow! Real Magic!](<https://www.curseforge.com/minecraft/mc-mods/snow-real-magic>)

# [0.5.2-beta2] - 2025-02-21

## Added

- Add config option to disable RegionMarker creation through renaming a Stick in an Anvil
- Add compat with [Snow! Real Magic!](<https://www.curseforge.com/minecraft/mc-mods/snow-real-magic>) - closes [Issue #152](<https://github.com/Z0rdak/Yet-Another-World-Protector/issues/152>)
- Add new `playerHasBypassPermission(IProtectedRegion, Player);` in `de.z0rdak.yawp.api.permission`.

## Changed

- (API) Reworked and moved around some helpers for flag checking

## Fixed

- (Fabric) Fix `spawning-all` flag not excluding item entities. This fixes item deleting when using this flag in combination with the `item-drop` flag.
- Fix item entities or entities like armor stand, paintings, etc. de-spawning when adding `spawning-all` flag to a region.
- Renaming a stick in an Anvil does not consider the input name and will always result in a RegionMarker
- Fix missing info logging for config options
- Fix owners and members of child region not bypassing flag if parent denies the same flag

# [0.5.2-beta1] - 2025-02-14

## Added

- Contributions guideline (CONTRIBUTING.md)
- (**API**) Add getting started section in README
- (**API**) Add usage section in wiki
- Added new flags: 
  - `keep-inv`
  - `keep-xp`
  - `no-item-despawn` [PR#142](<https://github.com/Z0rdak/Yet-Another-World-Protector/pull/142>)
  - `no-hunger`
- Added additional command for region deletion in local region commands: `/yawp local <dim> <region> delete -y`

## Changed

- Disable flag check/result logging by default
- Improve hint and add link to add flag when it does not exist in region

## Deleted

- Remove `/wp` command alternative and switch to just `/yawp` as the base cmd. This prevents trouble regarding the config and command registration and as well as questions why commands not work (looking at you JourneyMap!)
- Remove `wp_root_command` config option from `yawp-common.toml`

## Fixed

- Fix project setup not working on MAC due to missing dependencies. Added minecraft repo, to include lwjgl files to allow booting. [PR#143](<https://github.com/Z0rdak/Yet-Another-World-Protector/pull/143>)
- Fix RegionMarker not resetting
- (**Breaking change**) Fix typo in snow-smelting flag. It's not properly called `snow-melting`. You'll need to add this flag again in your regions.
- Fix region data dump causing errors when size is too big. For now dump is limited to a certain region size.

# [0.5.1-beta4] - 2025-02-09

## Fixed

- (Fabric) Fix place-block flag target position being calculated incorrectly
- Fix explosion flag causing npe
- Fix meele-* flags using wrong position for check
- Fix no-pvp flag having the wrong position for check and messaging the wrong player
- (Fabric) Fix missing UpdateArea event implementation

# [0.5.1-beta3] - 2025-01-27

## Fixed

- Fix NPE when updating area

# [0.5.1-beta2] - 2025-01-26

## Fixed

- Fix NPE causing crash on explosions

# [0.5.1-beta1] - 2025-01-23

## Added

- Add language support for euskara (aka. basque) and spanisch. Thanks to Jaie55#8188 for this contribution

## Changed

- Update russian language - thanks again Reincarnaciya

## Fixed

- Fix neoforge crashing due to mixin error
- Fix explosion flags not working (this time for real)

# [0.5.0-beta2] - 2025-01-20

## Fixed

- Fix old flags not being automatically removed during loading
- (Fabric) Fix explosion flags not working

# [0.5.0-beta1] - 2025-01-18

This update for **1.20.1+** is firstly build using the multi-loader project setup. With this the file name format changed

- from `yawp-<mc-version>-<yawp-version>-<modloader>.jar`
- to `yawp-<mc-version>-<modloader>-<yawp-version>.jar`

To make this multi-loader project setup possible it was necessary to restructure huge parts of the mod. But it brings
some great advantages for the future:

- Code Reusability: Core features and logic are now unified across mod loaders Forge and Fabric (and 1.21+ NeoForged),
  reducing duplication of effort
- Unified Development: Streamlined development process with a single codebase, simplifying version control and updates
  for both platforms
- Faster Development: Platform-agnostic features are implemented once, speeding up development across mod loaders
- Shared Resources: Assets, configs, and data files are now shared across mod loader platforms, reducing project size
  and redundancy

## Added

* Add new flags: `no-sign-edit`, `snow-melt`, `snow-fall`, `fluid-flow`, `lava-flow`, `water-flow`. Thanks to Magnus Jensen aka. legenden#7526!
* New iteration of the programming API. This includes new events, an API to query regions, change them and save them.
* Added new flag events:
  * `AddFlagEvent`, fired when a flag is added to a region. This is just an info event, its values can't be changed, and
    it can not be canceled.
  * `RemoveFlagEvent`, fired when a flag is removed from a region. This is just an info event, its values can't be
    changed, and it can not be canceled.
  * `UpdateFlagMessageEvent`, fired when the flag message for a flag is changed. This can be used to edit the message to
    implement e.g. a profanity filter.
* Add and exposed first iteration of a Permission API
* Exposed command constants in api package
* Expose API to build YAWP console commands
* Add Builders to create Local Region, Area and Flag instances.
* Add API to get Global and query/manage Dimensional Regions
* Add Dimensional Region API to query and manage Local Regions of a dimension
* Add builder to easily create multiline and pagination text components. This might be further polished and extracted into the API. Currently, this is located in `de.z0rdak.yawp.util.text`.
* Add new mod logo

## Changed

* Switch whole project to use the official mojang mapping instead of yarn mappings
* Update some flag lang keys to highlight the flag name in blue color
* Overhaul building of text components for the CLI by adding a whole abstraction layer.
* Separated building of text components from building CLI links, removing many code duplicates.
* Ditched forge suggestion for versioning schema. It's now back to https://keepachangelog.com/en/1.0.0/
* Change configs from server to common type. This means that config should no longer be synced to clients. As it should be. The clients will have the config by default, but it will be useless for them.

## Fixed

- Fix animal and monster check to exclude/include monster horses correctly
- Fix `explosions-entities` and `explosions-blocks` not working correctly
- Fix `fall-damage-players` not working correctly
- Fix `no-pvp` flag not checking for damage source to be a player

## Removed

* Remove old mod logo
* Remove `other-explosion-blocks` flag (was redundant)
* Remove `other-explosion-entities` flag (was redundant)

# [0.0.4.1-beta5] - 2024-10-09

## Fixed

- Fix error `This position is not loaded` when trying to create a region or changing region area.

# [0.0.4.1-beta4] - 2024-10-04

## Fixed

* [Fabric] Fix NPE caused by null entity in mixin for ServerLevel::explode

# [0.0.4.1-beta3] - 2024-10-04

## Added

* Add option to only log certain FlagStates for FlagCheckResults (Allowed and/or Denied)

## Changed

* Formatting of flag messages can now be done by using `&` and `§` as prefix. E.g.:
  * `"You are &cnot&r allowed to break blocks here, &9{player}&r!"` or
  * `"You are §cnot§r allowed to break blocks here, §9{player}§r!"`

## Fixed

* [Fabric] Fix 'spawning-*' flags preventing all entities from spawning
* Fix wrong FlagCheckResult state being logged

# [0.0.4.1-beta2] - 2024-09-23

## Fixed

* [Fabric] Fix `use-elytra` flag not working
* [Fabric] Fix `set-spawn` flag not working
* [Fabric] Fix `sleep` flag not working

# [0.0.4.1-beta1] - 2024-09-20

## Added

* Add Traditional Chinese localization. Thank you very much @dirttw!
* Debug logging for flag checks and most player related flags.
* Add config for logging. You are able to log only certain flags or flag categories.
* [Fabric] Add `place-fluid` and `scoop-fluid` flag, bringing flag consistency with forge version of the mod.

## Changed

* The `lightning` flag now also prevents lightning strikes not hitting entities, prevents creation of fire blocks and
  de-oxidation of copper blocks. These additional features may be split in separate flags down the road.
* The `access-container` flag now considers all entities implementing the vanilla inventory (like Minecarts with
  chests).
* The mod file now includes the license, readme, credits and this changelog.

## Fixed

* Fix bug which prevented removing from child regions with sphere area type
* Fix undo link for flag state setting
* Fix incorrect MAX and MIN defaults for expand command to be correct for the corresponding minecraft version.
* Fix some smaller CLI issues and typos
* [Fabric] Fix `exe-command` flag not working
* [Fabric] Fix `place-block` flag preventing interacting with entities/blocks.
* [Fabric] Fix inventory desync when using `place-blocks` flag.

# [0.0.4.0-beta2] - 2024-08-18

## Changed

* The error message for creating a region now specifies whether the parent region is unsuitable due to permission issues or because it doesn't contain the child region.

## Fixed

* Fix ResourceLocation/Identifier entries in `covered_block_entities` not correctly considered in some versions (1.20+).
* Tags included in the list `covered_block_entity_tags` inside the config `yawp-flags.toml` are now considered for the `break-blocks` and `place-blocks` flag
* Fix RegionMarker not working properly
* Fix NPE when checking for player related flags which caused (beside others) issues with Minecolonies
* Fix NPE when checking for mob-griefing through a projectile from an already dead entity (e.g. fireball from a ghast)
* Fix display of wrong flag state in CLI

# [0.0.4.0-beta1] - 2024-05-25

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

* Add commands to copy and clear region properties
* Add enhanced flag management and messages
* Add flag inheritance and overriding for regions
* Add new Local Region shape: Sphere
* Add the Global Region. It's the parent region of all Dimensional Regions. **One region to rule them all!**
* API: New events for flag checks. You can now listen to flag checks and cancel them if needed and listen for the
  result of a check and manipulate the outcome.
* Add new config options

### Copy region properties

* Add new command to copy properties from one Local Region to another Local Region:
  * `/wp dim <dim> <region> copy flags to-local <target-dim> <target-region>`: copy all flags from `region`
    to `target-region`
  * `/wp dim <dim> <region> copy flags to-dim <target-dim>`: copy all flags from `region` to `target-dim`
  * `/wp dim <dim> <region> copy state to-local <target-dim> <target-region>`: copy the region state from `region` and
    apply it to `target-region`
  * `/wp dim <dim> <region> copy state to-dim <target-dim>`: copy the region state from `region` and apply it
    to `target-dim`
  * `/wp dim <dim> <region> copy players to-local <target-dim> <target-region> [group] `: copy all players
    from `region`
    to `target-region`. To copy only a specific group (members, owners) add it as optional parameter
  * `/wp dim <dim> <region> copy players to-dim <target-dim> [group] `: copy all players from `region`
    to `target-dim`.
    To copy only a specific group (members, owners) add it as optional parameter

* Add new commands to copy properties from one Dimensional Region to another Dimensional Region
  * `/wp dim <dim> copy flags to-local <target-dim> <target-region>`: copy all flags from `dim` to `target-region`
  * `/wp dim <dim> copy flags to-dim <target-dim>`: copy all flags from `dim` to `target-dim`
  * `/wp dim <dim> copy state to-local <target-dim> <target-region>`: copy the region state from `dim` and apply it
    to `target-region`
  * `/wp dim <dim> copy state to-dim <target-dim>`: copy the region state from `dim` and apply it to `target-dim`
  * `/wp dim <dim> copy players to-local <target-dim> <target-region> [group] `: copy all players from `dim`
    to `target-region`. To copy only a specific group (members, owners) add it as optional parameter
  * `/wp dim <dim> copy players to-dim <target-dim> [group] `: copy all players from `dim` to `target-dim`. To copy
    only
    a specific group (members, owners) add it as optional parameter

### Enhanced Flags

* Flags now have their own dedicated flag message which is shown when the flag is triggered.
* It's now possible to mute flag messages for each individual flag (disabling the alert of the region will still mute
  all flags for the region).
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
* The flag message examples are listed in the language file to enable I18n support for the examples.

* Flags now have a *FlagState* instead of just being present/absent. When you add a flag, it will have the denied state
  to keep the same behavior as before. The different flag states are described as follows:
  * *Allowed* - The flag is allowed for the region and will be checked.
  * *Denied* - The flag is denied for the region and will be checked.
  * *Disabled* - The flag is disabled for the region and will not be checked.
  * *Undefined* - The flag is not defined for the region.
* Flags can be disabled to keep the flags in the region but disable the flag check. This is useful when you need to
  disable a flag but don't want to lose the flag settings (flag message, muted state, etc.).
* Add commands for enhanced flag management:
  * `/wp flag local <dim> <local> <flag> state <Allowed|Denied|Disabled>` - set the state for a flag
  * `/wp flag local <dim> <local> <flag> override <true|false>` - sets the flag to override the same flag in child
    regions
  * `/wp flag local <dim> <local> <flag> msg set <msg>` - set a new message for the flag. Check the wiki for a
    description of possible placeholders for messages.
  * `/wp flag local <dim> <local> <flag> msg clear` ...
  * `/wp flag local <dim> <local> <flag> msg mute` ...

* Also added a command to list the region flags to the flag command
  * `/wp flag local <dim> <local>` - Lists all flags of the region

The same commands of course work for Dimensional and the Global Regions:

* `/wp flag dim ...` to manage flag properties for a Dimensional Region
* `/wp flag global ...` to manage flag properties for the Global Region

### Flag inheritance & overriding for regions

* Child regions now inherit the flags of their parent regions. This means that every region will also inherit the flags
  of the corresponding Dimensional region and the Global Region.
* Parent regions can now override flags of child regions to enforce flags onto them.
* Flag pagination now includes parent flags (links to parent flags and the parent itself are shown behind the flag
  name). The region type is indicated by an indicator. **G** for Global, **D** for Dimensional and **L** for Local
  Regions.
* Flag list links for regions now also show number of flags from parent regions, which are considered for region checks
  in parentheses.
* The commands to list the flags of a region (`/wp ... list flag`) are now prompting all responsible flags of the
  region, including the flags of the parent regions.
* A new command has been added to only prompt the flags only of the supplied region
  * `/wp ... list region-flag`
* Flag pagination now sorts flags by flag state and then by alphabetical order.
  * Green - allowed flags
  * Red - denied flags
  * Gray - disabled flags
* Flags now have an internal category. This info will also be added to the wiki. The categories are a change to allow
  some other features to be added in the future. The categories are not final and may still change.

### Sphere Local Region

* Sphere Local Regions are now available. The sphere is defined by the center and the radius. The radius can either be
  defined by a BlockPos or the radius size. Note that the radius is not counting the center block. Because of this, it
  is only possible to create spheres with an odd diameter.
* A sphere with radius 0 will only cover the center block.
* Add commands to create a Sphere Local Region:
  * `/wp local <dim> create <region-name> Sphere <center-pos> <radius-pos> [<parent-region>]`
  * `/wp local <dim> create <region-name> Sphere <center-pos> <radius> [<parent-region>]`
* You can change the area type of the region area at any time from a Cuboid to a Sphere and back.
* Support to create Spherical Regions with the RegionMarker will be added soon

### Config

* `yawp-common.toml` - Add new permission config:
  * `allow_region_tp` - Decides whether teleporting inside/outside a region is allowed for everyone. Useful when using
    Waystones in regions for example.
  * `disable_cmd_for_non_op`. Defines whether mod commands are disabled for non-OP players. This is useful when you
    want
    to use the mod only for OPs or players which have their UUID entry in the config. Enable this if you don't want
    the
    YAWP command to be seen by non-permitted users.
  * `hierarchy_ownership`: Defines whether the region hierarchy should be considered for region ownership. If enabled,
    ownership of parent regions will automatically (implicit) apply to child regions. By default, this is set to true.
* `yawp-flags.toml` - Add new flag configs:
  * `remove_entities_for_spawning_flags`: Toggles the de-spawning of entities when using the `spawning-*` flags.
* `break_flag_entities` and `break_flag_entity_tags` are now also used for the `place-blocks` flag.

### Global Region

* The Global Region has the same properties as the Dimensional Regions but is not limited to one dimension. It is the
  parent region of all Dimensional Regions and considered everywhere for flag checks.
* Add new commands for management of the Global Region:
  * `/wp global info`.
  * `/wp global clear flags|players|teams|group`.
  * `/wp global add player|team|flag`.
  * `/wp global remove player|team|flag`.
  * `/wp global list flag|region-flag|group|dim`.
  * `/wp global state alert|enable`.
  * `/wp global reset`.
  * `/wp global ...`
* Add interactive CLI support for the Global Region

### API

* Add FlagCheckEvent. This event is fired before a flag is checked. It can be canceled and in the progress can be used
  to cancel the flag check entirely.
* Add FlagCheckResult as an event. This event is fired after a flag check. It can be used to manipulate the
  outcome of the flag check. This event cannot be canceled.
* Add UpdateArea event as subtype of RegionEvent. This event is fired whenever the area of a region is updated. It can
  be
  canceled to prevent the update.
* Add RenameRegion event as subtype of RegionEvent. This event is fired whenever a region is renamed. It can be
  canceled to prevent the renaming.

### Commands

* You can now add multiple flags at once by using the new command syntax:
  * `/wp ... add flags <flag1> <flag2> ... <flagN>`
  * `/wp ... add all-flags`
* You can also remove multiple flags at once using the new command syntax:
  * `/wp ... remove flags <flag1> <flag2> ... <flagN>`
* Add interactive CLI support for enhanced flag management
* Add new command to expand the area of a Local Region:
  * `/wp local <dim> <local> area expand Cuboid [yMin] [yMax]`.
    * The optional parameters can be used to set a specific height.
    * Omitting the parameters will set the region area to the Minecraft version specific build limits
  * `/wp local <dim> <local> area expand Sphere [<inc>]`.
    * The optional increment can be used to set increment the radius by a specific amount (negative or positive)
    * Omitting the parameter will increase the radius by 1.
* Add new command to rename a Local Region: `/wp local <dim> <local> rename <newName>`.
* Add new commands to add and remove offline players from/to regions.
  * `/wp local <dim> <local> add player <group> by-name <player names separated by space>`.
  * `/wp local <dim> <local> remove player <group> by-name <player names separated by space>`.
  * `/wp local <dim> <local> add player <group> by-uuid <player uuid>`.
  * `/wp local <dim> <local> remove player <group> by-uuid <player uuid>`.
  * Note that you can define multiple names for adding and removing but only one UUID at a time.
  * Same goes for the Global and Dimensional Regions as well.
* There are new commands to clear players, teams, and flags for all region types.
  * `/wp dim <dim> clear players|teams|flags|group ...`
  * `/wp global clear players|teams|flags|group ...`
  * `/wp local <dim> <region> clear players|teams|flags|group ...`
* Add new command to enable/disable all Local Regions in a dimension:
  * `/wp dim <dim> state enable-local true|false`
* Add new command to enable/disable flag alert messages for all Local Regions in a dimension:
  * `/wp dim <dim> state alert-local true|false`
* Add new command to delete all Local Regions in a dimension:
  * `/wp dim <dim> delete-all regions forever seriously`
* Add new command to reset the Dimensional Region:
  * `/wp dim <dim> reset dim`
* Add new command to reset all Local Regions in a Dimensional Region:
  * `/wp dim <dim> reset regions`

## Changed

* YAWP can now be used in single-player (open to LAN) worlds. It is still a server-side only mod, but now also works for
  integrated servers / LAN worlds and single player worlds.
* Reworked the layout and design of the CLI to be more user-friendly and consistent for Local, Dimensional and Global
  Region types where possible.
* Change flag pagination for regions to include a link to the flag info as well as some quick links.
* Dimensional Regions now can be muted (as well as their flags, all or individually)
* Change Dimensional Region CLI to include support for muting regions.
* Rename spatial properties to area properties. This change also involves some commands and language keys.
* Rearranged the area properties CLI page, trying to give a more concise overview.
* Change command to update Local Region area:
  * Old: `/wp region <dim> <region> area Cuboid <pos1> <pos2>`
  * New: `/wp local <dim> <local> area set Cuboid <pos1> <pos2>`
* When creating a Local Region you can no longer supply the owner of the region. Instead, you can optionally supply the
  region parent:
  * e.g.: `/wp local <dim> create <region-name> Cuboid <center-pos> <radius-pos> [<parent-region>]`
  * The argument will try to suggest only owned regions that fully include the new region. This will also be checked
    when executing the command. If the provided region is invalid, the Dimensional Region will be used instead.
  * Omitting the parent, will also set the Dimensional Region as parent
* Creating a Local Region with the RegionMarker and the correlated command now also requires you to provide the parent
  region.
  * If there are multiple valid options available, you are required to select one.
  * If no parent is provided, the Dimensional Region will be used as parent.
* Improved RegionMarker indicators for marked blocks. It's item name now shows colored indicators for
  * the amount of blocks which needs to be marked for a valid area
  * a selected teleport position
* The RegionMarker now also prompts feedback for marked blocks and a valid area to the player.
* Rename affiliation to groups. This change also involves some commands and language keys.
* Change commands to manage groups (former known as affiliations):
  * Old: `/wp region <dim> <region> add|list|remove affiliate player|team <member|owner> ...`
  * New: `/wp local <dim> <local> add|list|remove group player|team <member|owner> ...`
* Change commands to manage Local Regions:
  * Old: `/wp region <dim> <region> ...`
  * New: `/wp local <dim> <local> ...`
* The spawning flags no longer remove entities with the PersistanceRequired tag or a custom name.
* Renaming a Stick to create a RegionMarker is now disabled to prevent permission issues. This will come back in a
  future update with an overhaul of the RegionMarker.
* Moved the region name examples (used when creating a new region) to the language file to enable I18n support for the
  examples.
* Changed license from LGPL v3 to AGPL v3.
* Change Dimensional Region state CLI to toggle the state (enable and alert) of all Local Regions in the dimension.
* Rename config value `break_flag_entities` to `covered_block_entities`
* Rename config value `break_flag_entity_tags` to `covered_block_entity_tags`
* The region add flag command now has a new syntax. The state and override flag are optional and default to _Denied_ and
  _false_.
  * Old: `/wp ... add flag <flag>`
  * New: `/wp ... add flag <flag> [state] [override]`

## Removed

* Remove UpdateRegionEvent introduced in 0.0.3.0-beta1. This is replaced by the UpdateArea and RenameRegion events.

## Fixed

* Fix some cli typos
* Fix parent - child hierarchy inconsistency when deleting children
* It is no longer possible to delete regions which have a Local Region as parent
* Lang key fixes... well more like a whole overhaul of the language keys
* Fix region teleport command not working properly for non-ops
* Fix entry point for ignite-explosive flag mixin
* Fix ignite-explosive flag not considering explosions not caused by players
* The command `/wp marker create ...` and `/wp dim <dim> create local ...` now properly checks for permission for the
  parent region

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

* Bump forge version to latest

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

* [Fabric] Add `mob-griefing`, `enderman-griefing` and `zombie-destruction` flag. Ported by petersv5 aka. ptefar -
  thanks a lot!

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

* Fix YAWP commands being unregistered upon using `/reload`. This fixes the issue with using Ice & Fire together with
  YAWP (#75).
* Fix YAWP resource pack not loading correctly at client side.
* Fix `ignite-explosives` causing the server to crash.

# [0.0.2.7-beta2] - 2023-04-09

## Fixed

* Fix YAWP flags not working due to configuration errors (damn mixins!)

# [0.0.2.7-beta1] - 2023-03-29

From now on this changelog will use the keep a changelog format: https://keepachangelog.com/en/1.0.0/. YAWP is still
following the schematic versioning suggested by
forge: https://docs.minecraftforge.net/en/1.19.x/gettingstarted/versioning/.

## Added

* CLI: Region state quick-link & command enhancements: `/wp region <dim> <region> state enable|alert`
  and `/wp dim <dim> enable` now can be used without providing the new state (you can still provide the state explicitly
  though). They work as a toggle used this way. The related quick-links for Dimensional and Local Regions have been
  adjusted accordingly.
* CLI: Undo links. Most command confirmation messages will now provide a link to undo the action. (#63)
* Spawning-related flags now despawns all entities in the Local Region, which are covered by the flag.
* Spawning-related flags now despawns all entities in the Dimensional Region, which are covered by the flag (exluding
  entities in the Local Regions without the flag).

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

* Flag `entity-place`. It is replaced by the `enderman-griefing` flag, which covers picking and placing of blocks by
  endermen.

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
* Add flag `enter-dim` for Dimensional Regions. This flag prevents players from traveling (using a portal or
  teleporting) to the dimension which has this flag active.
* Add flag `mob-griefing` to prevent mob griefing.
* Add flag `enderman-griefing` to prevent enderman from picking up and placing blocks. This replaces the now removed
  flag `entity-place`.
* Add pagination support for flags, regions/children, players and teams for the CLI
* Add config option for amount of entries (flags, regions, players or teams) per page: `cli_entries_per_page`. It's
  located in the `yawp-region-defaults.toml`. It defaults to 5 entries per page.
* Reintroduced the config option to choose the mods root command: `wp_root_command`. It's located in
  the `yawp-common.toml` config. You are able to choose between `/wp` and `/yawp`, with `/wp` being the default.

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

* Reworked header format (the lines with == ... ==) to be more consistent and add a self-link to each header for easier
  accessibility.
* Remove `use` flag.

## Fixes

* Fix priority not set properly for Local Regions
* Fix some I18n typos and missing translations

## Notes

* Currently child regions do not overwrite flags of their parents. This means for example when having a parent with the
  break-block flag, which denies breaking blocks, and a child which doesn't have this flag, the flag of the parent still
  covers the child region.

# 0.0.1.3-beta4

## Fixes

* Fix mod commands being executable without permission when using new alternative base command yawp

# 0.0.1.2-beta4 - I18n and flag fixes

## Additions

* Add link to Local and Dimensional Region info header to copy region NBT data to clipboard for more easy
  troubleshooting
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

This update brings the first iteration of working Local Regions. Due to some drastic changes in saving and loading
region data, older regions are not supported - sorry!
Rest assured, once the mod goes from beta to release, these kinds of changes regarding will be kept to a minimum to
ensure backwards compatibility.

## Additions

* Local Region flags are now implemented! Please visit the wiki for a full list of flags and for more information about
  how the Region hierarchy works.
* Add RegionMarker stick. Use it to mark an area for a new region and then either use the CLI to create the region or
  rename the stick in an anvil.
* Add sub-command to give a player a RegionMarker: `/wp marker give [<player>]`.
* Add sub-command to reset the state of a RegionMarker the player is holding: `/wp marker reset`.
* Add flag for knock-back protection against players.
  To learn more about the priorities for overlapping regions and region hierarchy please visit the wiki.
* Add flag `spawning-slime`, `spawning-villager`, `spawning-trader`, `drop-loot-player`. Visit the wiki for detailed
  information about the flags.

## Changes

* Spherical Regions are for now disabled. They will come back later with other shapes.
* When removing a child region from a Local Region, its new parent will be the Dimensional Region and the child will be
  disabled.
* The flag command `/wp flag ...` is disabled for now. It has currently no real functionality and will come back in the
  next update.
* Deleting regions no longer works for regions with children. There will be a config option in a future update to enable
  this, though.
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

This is the first beta release of this mod. Please be so kind and report any bugs you encounter on the GitHub issue
page: https://github.com/Z0rdak/Yet-Another-World-Protector/issues.
If you have suggestions, feel free to post them, too.

* Add online wiki as a guide on how to use the mod: https://github.com/Z0rdak/Yet-Another-World-Protector/wiki
* Add dimensional regions
* Add configuration for default region flags, CLI permission,
* Add many flags to protect your server (visit the wiki for more info)
# Overview

Yet Another World Protector, or YAWP for short, is a server-side mod which allows you to protect your creations against various different events/actions of players, mobs and the environment. It was inspired by the forge mod [WorldProtector](https://www.curseforge.com/minecraft/mc-mods/worldprotector) and the [WorldGuard](https://dev.bukkit.org/projects/worldguard) plugin.

***The mod is currently in its beta state, with many features not yet implemented but to come. These features will include most of the things you know from WorldProtector and WorldGuard.***

This mod will be released for Minecraft versions 1.16.5+. If you are living in the past and are looking for a version for Minecraft 1.12.2, I suggest looking into using [WorldDefender](https://www.curseforge.com/minecraft/mc-mods/world-defender).


# Regions

In YAWP there are different kinds of regions to protect your server: Dimensional Regions and normal, local Regions. Dimensional Regions are special regions which are tied to one dimension - like gamerule-flags for different dimensions - and normal, local Regions do have a spatial property which they are limited to. 

The properties of regions, flags, affiliation, etc. can all be managed through an interactive command line interface (see images below). The colored texts indicate shortcuts for easier region management. Clicking on them gives the player a suggestion for executing a command or sometimes straight execute the command which is hinted by the hover text. 

## Flags

You can add rules to Dimensional or normal Regions with flags. If the flag is defined, it prevents the defined action associated with the flag.
See the [Flags](https://github.com/Z0rdak/Yet-Another-World-Protector/wiki/Flags) page for more details about the different flags.

There will be more complex flags in the future, like conditional flags, flags to define a list of allowed blocks/entities/items/... and much more.

## Affiliation

Regions can have owners and normal members. Owners are allowed to manage the region and bypass the flags, members are only allowed to bypass the flags set in the region.

Owners/Members can be individual players or a [team](https://minecraft.fandom.com/wiki/Commands/team).

There are more granular features planned for region affiliation in the future - each affiliation with different permissions.

## [Dimensional Regions](https://github.com/Z0rdak/Yet-Another-World-Protector/wiki/Dimensional-Regions)

Dimensional Regions are a special kind of regions because they have no spatial restrictions. Dimensional Regions work like normal regions, but are tied to a dimension. They can have owners, members, flags and can be activated and deactivated.

Dimensional Regions can not be created manually. They are in fact automatically generated with each new dimension. The first Dimensional Region for the overworld is created when the server is created. The following Dimensional Regions are created whenever a player travels to a new dimension, thus creating the new dimension with their respective Dimensional Region.

![](https://cdn.modrinth.com/data/py6EMmAJ/images/9f9dd2c8c5d2f82ded6fca9f73ef0dd161dc35d1.png)

Dimensional Regions can be managed only by their owners, or by players which have the required OP level or have an entry in the configuration. For more information, visit the wiki.
## Local Regions

With Local Regions you are able to define areas in your world, which you want to protect from certain actions/events which are caused by players, other entities or the environment. You are able to define separate owners and members for owners by assigning vanilla teams and specific players to the region. The area can be defined (and changed) in different geometric shapes. E.g. Cuboid, Sphere and more to come.
Furthermore, a region can have child regions and a parent region which are considered with their corresponding flags. They also have a priority to manage overlapping regions. 

NOTE: Local regions are in a preview state. Beta release 0.0.1.0-beta2 introduces local regions like the one you know from WorldProtector, with all their properties. The checks for the region flags are not yet in place. You can still define flags, members, owner, area, etc. to a region, but the flags will not prevent players from doing 'bad' things. Because the flag checks now involve also the dimensional regions and will be even more complicated in the future, this implementation is coming in the next update.

![](https://cdn.modrinth.com/data/py6EMmAJ/images/12e4cd2e56a77247e2e7b8d6212c84ab6b29276b.png)

## Planned features

There are a bunch of planned features on the development roadmap. The first goal for the official version 1.0 release is to cover all features from the [WorldProtector 1.16.5 port](https://github.com/Z0rdak/WorldProtector).

After that, there are many more features planned. The following list is not comprehensive. Features marked with an Asterix are already partly implemented, but not yet available in the beta:

* Regions with different shapes (Cylinder, Prism, 3DPolygon)*
* The good old Region Marker*
* Region templates for easier managing same regions with different spatial properties
* Different flag (Conditional, List, ...) types*
* Region triggers for entering/leaving a region (for displaying messages, executing commands, etc)*
* Define your own region affiliation for more granular management of regions*
* More granular permission system (combined with the more granular affiliation features)
* API to manage regions for other mods
* Events for common regions actions (e.g. creating, deleting regions, adding owners, etc)
* ...
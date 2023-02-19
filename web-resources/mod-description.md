# Overview

Yet Another World Protector (YAWP) is the admin tool to protect your minecraft server!

It allows you to protect your creations against various different events/actions of players, mobs and the environment.
It was inspired by the forge mod [WorldProtector](https://www.curseforge.com/minecraft/mc-mods/worldprotector) and
the [WorldGuard](https://dev.bukkit.org/projects/worldguard) plugin.

![](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/1.16.x/web-resources/yawp-feature-1.png)

***The mod is currently in its beta state, with many features not yet implemented but to come. These features will
include most of the things you know from WorldProtector and WorldGuard.***

This mod will be released for Minecraft versions 1.16.5+. If you are living in the past and are looking for a version
for Minecraft 1.12.2, I suggest looking into
using [WorldDefender](https://www.curseforge.com/minecraft/mc-mods/world-defender).

___

![Pagination](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/1.16.x/web-resources/yawp-feature-2.png "Interactive CLI")

* Pagination for more easy management of flags, players, regions, etc.

![Pagination](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/1.16.x/web-resources/yawp-cli-pagination.png "Pagination")

* Region CLI

![](https://cdn.modrinth.com/data/py6EMmAJ/images/12e4cd2e56a77247e2e7b8d6212c84ab6b29276b.png)
___

![Pagination](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/1.16.x/web-resources/yawp-feature-3.png "Region types")

In YAWP there are different kinds of regions to protect your server: Dimensional Regions and normal, local Regions.
Dimensional Regions are special regions which are tied to one dimension - like gamerule-flags for different dimensions -
and normal, local Regions do have a spatial property which they are limited to.

The properties of regions, flags, affiliation, etc. can all be managed through an interactive CLI. The colored texts
indicate shortcuts for easier region management. Clicking on them gives the player a suggestion for executing a command
or sometimes straight execute the command which is hinted by the hover text.

## Local Regions

With Local Regions you are able to define areas in your world, which you want to protect from certain actions/events
which are caused by players, other entities or the environment. You are able to define separate owners and members for
owners by assigning vanilla teams and specific players to the region.

The area can be defined (and changed) in different geometric shapes. ~~E.g. Cuboid, Sphere and more to come.~~ For now
limited to Cuboid shape.

![Pagination](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/1.16.x/web-resources/yawp-feature-5.png "")

Furthermore, a region can have child regions and a parent region, which are considered with their corresponding flags.
They also have a priority to manage overlapping regions.

## Dimensional Regions

Dimensional Regions have no spatial restrictions. Dimensional Regions work
like normal regions, but are tied to a dimension. They can have owners, members, flags and can be activated and
deactivated.

Dimensional Regions can not be created manually. They are in fact automatically generated with each new dimension. The
first Dimensional Region for the overworld is created when the server is created. The following Dimensional Regions are
created whenever a player travels to a new dimension, thus creating the new dimension with their respective Dimensional
Region.

Dimensional Regions can be managed only by their owners, or by players which have the required OP level or have an entry
in the configuration.
For more information, visit the wiki.
___
![Pagination](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/1.16.x/web-resources/yawp-feature-4.png "Region affiliation")

Regions can have owners and normal members. Owners are allowed to manage the region and bypass the flags, members are
only allowed to bypass the flags set in the region.

Owners/Members can be individual players or a [team](https://minecraft.fandom.com/wiki/Commands/team).

![Pagination](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/1.16.x/web-resources/yawp-feature-6.png "")

These affiliations allow you to create regions and assign players as their owners. They can then manage their regions
themselves and
add other players to them, as well as create their own sub-regions within their regions.

If you have trouble with setting up regions, visit the discord for help.

There are more granular features planned for region affiliation in the future - each affiliation with different
permissions.

___
![Pagination](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/1.16.x/web-resources/yawp-feature-7.png "")

You can add rules to Dimensional or Local Regions with flags. If the flag is defined, it prevents the defined action
associated with the flag.
See the [Flags](https://github.com/Z0rdak/Yet-Another-World-Protector/wiki/Flags) page for more details about the
different flags.

There will be more complex flags in the future, like conditional flags, flags to define a list of allowed
blocks/entities/items/... and much more.
___
![](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/1.16.x/web-resources/yawp-feature-8.png)

The commands of the interactive CLI are also designed to be used by the server console and can be executed by command
blocks.
This allows you to automate region setups, add/remove players, enable or disable regions, etc.
___
![](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/1.16.x/web-resources/yawp-feature-9.png)

The client side installation of the mod supports Internationalization for german, english and russian.

If your language is not yet supported, feel free to reach out to me via discord or issue a pull request via github! :)
___
![](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/1.16.x/web-resources/yawp-feature-10.png)

The configuration of the mod allows you to control which is allowed to use the mod, sets default flags for Local and
Dimension Regions, command line interface options, etc.
___
![](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/1.16.x/web-resources/yawp-feature-11.png)

The documentation of the mod is still small but growing, and I am trying to keep up with the content.

The documentation covers

* descriptions of different regions
* available commands
* available flags
* configuration
* ..

If something is not covered feel free to hop on our discord server and ask your question there. :)
___

![](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/1.16.x/web-resources/yawp-feature-12.png)

## Roadmap

The following list is not comprehensive. Features
marked with an * are already partly implemented, but not yet available in the beta:

* Regions with different shapes (Cylinder, Prism, 3DPolygon)*
* Region templates for easier managing same regions with different spatial properties
* Different flag (Conditional, List, ...) types*
* Region triggers for entering/leaving a region (for displaying messages, executing commands, etc)*
* Define your own region affiliation for more granular management of regions*
* More granular permission system (combined with the more granular affiliation features)
* API to manage regions for other mods
* Events for common regions actions (e.g. creating, deleting regions, adding owners, etc)
* ...
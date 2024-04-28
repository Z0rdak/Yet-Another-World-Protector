# Yet Another World Protector

Yet Another World Protector (YAWP) is _the_ admin tool to protect your minecraft server! Its available for both, (Neo-)
Forge and Fabric.

YAWP designed to be used on dedicated servers, but can also be used in single player worlds and worlds opened to LAN.
It allows admins (and players with assigned permission) to protect their creations against various events/actions of
players, mobs and
the environment.

Create regions for your builds and apply region flags to protect them.
Assign players or vanilla teams to regions, so they can manage their own regions.

YAWP was inspired by the forge mod [WorldProtector](https://www.curseforge.com/minecraft/mc-mods/worldprotector) and
the [WorldGuard](https://dev.bukkit.org/projects/worldguard) plugin.

Join our discord if you have any questions or suggestions: [YAWP Discord](https://discord.gg/d7hArKCUtm)

*** 

![](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/online-pages/web-resources/yawp-feature-1.png)

***The mod is currently in its beta state, with many features not yet implemented but to come. These features will
include most of the things you know from WorldProtector and WorldGuard.***

This mod will be released for Minecraft versions 1.16.5+. If you are living in the past and are looking for a version
for Minecraft 1.12.2, I suggest looking into
using [WorldDefender](https://www.curseforge.com/minecraft/mc-mods/world-defender).

*** 
![](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/online-pages/web-resources/yawp-feature-3.png "Region types")

In YAWP there are three different kinds of regions to protect your server:

- The Global Region
- Dimensional Regions
- Local Regions

## The Global Region

The Global Region is... well global. It sets rules/flags for all dimensions of the server and thus all Local Regions
within their corresponding Dimensional Regions.

## Dimensional Regions

Dimensional Regions are special regions which are tied to one dimension - like gamerule-flags for different dimensions.

Dimensional Regions have no spatial restrictions in the corresponding dimension. Dimensional Regions work like normal
regions, but are tied to a dimension.

They can't be created manually. They are in fact automatically generated with each new dimension. The first Dimensional
Region for the overworld is created when the server is created.

The following Dimensional Regions are created whenever a player travels to a new dimension, thus creating the new
dimension with their respective Dimensional Region. Modded dimensions are supported as well.

## Local Regions

Local Regions are the regions you most likely now from WorldGuard or WorldProtector. They have a defined area in which
they work in and which they are limited to.

With Local Regions you are able to create areas in your world, which you want to protect from certain actions/events
which are caused by players, other entities or the environment.

To create a Local Regions you will need to have the permission of the parent region or permission from the config.

[Local Regions can be created](https://github.com/Z0rdak/Yet-Another-World-Protector/wiki/Commands#creating-a-local-region)
by marking the area with a RegionMarker and executing the corresponding command or by just using a command without the
RegionMarker.

For now the area of a region can be defined as in a cuboid or spherical shape, but I am planning to add more shapes for
region areas in future updates.

***

## Properties of regions

The properties of regions, flags, groups, etc. can all be managed through
an [interactive CLI](https://github.com/Z0rdak/Yet-Another-World-Protector/wiki/Interactive-CLI).

The colored text indicates interactive shortcuts commands or command suggestions. All colored text have a tooltip hint
which explains shortly what clicking on the corresponding link does.

Clicking on them gives the player a suggestion for executing a command or sometimes straight execute the command which
is hinted by the hover text.

Regions can only be managed by their owners, or by players which have the required OP level or have an entry in the
configuration. For more information, visit the wiki.

![](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/online-pages/web-resources/yawp-feature-4.png "Region hierarchy")

***

TBD

![](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/online-pages/web-resources/yawp-feature-5.png "Region groups")

Regions have owners and normal members. Owners are allowed to manage the region and bypass the flags, members are
only allowed to bypass the flags set in the region.

Owners and Members can be individual players or a [team](https://minecraft.fandom.com/wiki/Commands/team).

Groups allow you assign specific region permissions to different players and teams. Currently, there are only two
default groups: **owners** and **members**.

***

![](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/online-pages/web-resources/yawp-feature-6.png "")

- **Owners** can manage their region themselves and add other players/teams to the region. They can also create their
  own sub-regions within their region, manage flags, ect. and of course owners bypass flags.

- **Members** are only allowed to bypass flags only.

I am planning on adding a [LuckPerms](https://www.curseforge.com/minecraft/mc-mods/luckperms) integration with a future
update, so you can define your own groups and permissions.

If you have trouble with setting up regions, visit the discord for help.

*** 

![](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/online-pages/web-resources/yawp-feature-7.png "")

Flags are the core of the region protection system. They define which actions are allowed or denied in a region. Flags
can be set to

- **Allowed**, to allow the corresponding action,
- **Denied**, to deny the corresponding action,
- **Disabled**, to disable the specific flag for flag checks.

Child regions inherit the flags from their parent regions. It is further possible to override the flag state for child
regions by setting the **override** property of the flag of the parent region accordingly.

See the [Flags](https://github.com/Z0rdak/Yet-Another-World-Protector/wiki/Flags) page for more details about the
different flags.

I am planning to add more flag in the future. A list of suggested flags which I'll slowly add to the mod can be found
here: [Flag suggestions](https://github.com/Z0rdak/Yet-Another-World-Protector/issues/66)

There will also be more complex flags in the future. Like *ListFlag* which will allow you to define a list of allowed
blocks/entities/.. which are forbidden in the context of a flag.

***

![](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/online-pages/web-resources/yawp-feature-2.png "Interactive CLI")

* Pagination for more easy management of flags, players, regions, etc.

![](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/online-pages/web-resources/yawp-cli-pagination.png "Pagination")

* Local Region CLI

![](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/online-pages/web-resources/local-interactive-cli-info.png)

*** 

![](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/online-pages/web-resources/yawp-feature-8.png)

The commands of mod are also designed to be used by the server console and can be executed by command blocks.

This allows you to automate region setups, add/remove players, enable or disable regions, etc.
***

API

***

![](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/online-pages/web-resources/yawp-feature-9.png)

The client side installation of the mod provides language support (I18n) in form of a resource pack. Currently, english,
german and russian language is supported.

Starting with mod version 0.0.2.9-beta2 and Minecraft Version 1.19.4, YAWP provides a default english translation.

If your language is not yet supported, feel free to reach out to me via discord or issue a pull request via github! :)

*** 
![](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/online-pages/web-resources/yawp-feature-10.png)

The configuration of the mod allows you to control which is allowed to use the mod, sets default flags for Local and
Dimension Regions, command line interface options, etc.

The configuration is split in different files, covering different topics.

- `yawp-common.toml` - config for command permissions
- `yawp-flags.toml` - config for flags, custom flag messages, etc
- `yawp-region-defaults.toml` - config for default region properties

Since YAWP is a server-side mod, the config for it is found in the directory `/serverconfig` in your minecraft world
directory.

*** 

[![YAWP Wiki](https://raw.githubusercontent.com/Z0rdak/Yet-Another-World-Protector/online-pages/web-resources/yawp-feature-11.png 'YAWP Wiki')](https://github.com/Z0rdak/Yet-Another-World-Protector/wiki)

The documentation of the mod is small but growing. I am trying to keep up with the content. You can find the
documentation [<< here >>](https://github.com/Z0rdak/Yet-Another-World-Protector/wiki).

The documentation covers

* descriptions of different regions
* getting started & FAQ
* available commands & flags
* configuration
* and more ...

If something is not covered feel free to hop on our discord server and ask your question there. :)
___

## Roadmap

There are many more features to come. The mod is in active development!
The following list is not comprehensive.

* Different flag types (List, Effect, ...) for more granular control
* Data pack driven flag configuration
* Region boundary visualization
* Define your own region groups for more granular management of regions (LuckPerms)
* More granular permission system (combined with the more granular group features)
* Region templates for easier managing same regions with different spatial properties
* Region triggers for entering/leaving a region (for displaying messages, executing commands, etc)*
* Regions with different shapes (Cylinder, Prism, 3DPolygon)*
* \<your suggestion here\>
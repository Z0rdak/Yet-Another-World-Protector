# Introduction

Yet Another World Protector, or YAWP for short, is a server-side mod which allows you to protect your creations against various different events/actions of players, mobs and the environment. It was inspired by the forge mod [WorldProtector](https://www.curseforge.com/minecraft/mc-mods/worldprotector) and the [WorldGuard](https://dev.bukkit.org/projects/worldguard) plugin.

***The mod is currently in its beta state, with many features not yet implemented but to come. These features will include most of the things you know from WorldProtector and WorldGuard.***

This mod will be released for Minecraft versions 1.16.5+. If you are living in the past and are looking for a version for Minecraft 1.12.2, I suggest looking into using [WorldDefender](https://www.curseforge.com/minecraft/mc-mods/world-defender).

# Commands

## Interactive commands

You can use the interactive links (indicated by the colored text) to copy commands into your chat or run them directly with a single click.
Just hover over the links and read the hints to know what they are doing.

Note: Journey Map also uses the ``/wp`` command. For this reason, YAWP offers ``/w-p`` or ``/yawp`` as alternatives.

## Command overview

Currently, there are four types of commands to manager regions:

- ```/wp region```: Define, update, remove, activate/deactivate, set region priority; List regions, query information,
  teleport to regions regions;

- ```/wp expand```: allows to modify the Y level (height) of the marked region.

- ```/wp flag```: allows to define one or several flags (rules) for a region to protect it.

- ```/wp player```: allows to add/remove players to/from regions

- ```/wp team```: allows to add/remove teams to/from regions

Use ```/wp help``` for more information. Or ```/wp help <command>``` for information about a specific command.

# Configuration

## Server

TBD

## Permissions

TBD

## Region defaults

TBD

# Flags

Currently, there are **57 flags** available. Click below to show the complete list.

To add a flag use the command ```/wp flag add <region> <flag>```. To remove a flag from a region
use ```/wp flag remove <region> <flag>. ```

To add or remove multiple flags at once just separate them with a space between them.

You are also able to add or remove all flags by using the special flag 'all': ```/wp flag <add|remove> <region> all```

<details>
  <summary> Flag list (click me):</summary>

- **break**: prevents players from breaking blocks and picking up fluids
- **place**: prevents players from placing blocks and fluids
- **ignite-explosives**: prevents explosives from blowing up
- **explosions-blocks**: prevents all explosions from destroying blocks
- **explosions-entities**: prevents all explosions from damaging entities
- **creeper-explosions-blocks**: prevents explosions caused by Creepers to destroy blocks
- **creeper-explosions-entities**: prevents explosions caused by Creepers to damage entities
- **other-explosions-blocks**: prevents all other explosions from destroying blocks
- **other-explosions-entities**: prevents all other explosions from damaging entities
- **tools-secondary**: prevents all type of secondary tool actions (strip wood, till farmland, create paths)
- **strip-wood**: prevents wood from being stripped
- **till-farmland**: prevents farmland from being tilled
- **shovel-path**: prevents creation of path blocks
- **trample-farmland**: prevents all farmland trampling
- **trample-farmland-player**: prevents players from trampling farmland
- **trample-farmland-other**: prevents non-player entities from trampling farmland
- **lightning**: prevents entities from being hit by lightning (or at least get hurt/transformed)
- **animal-taming**: prevents players from taming animals
- **animal-breeding**: prevents players from breeding animals
- **animal-mounting**: prevents players from mounting animals
- **spawning-all**: prevents spawning of all entities
- **spawning-monsters**: prevents spawning of monsters
- **spawning-animal**: prevents spawning of animals
- **spawning-irongolem**: prevents spawning of iron golems
- **spawning-xp**: prevents spawning of xp orbs completely
- **use**: prevents players to interact with most blocks like buttons, doors, pressure plates, etc.
- **use-bonemeal**: prevents players from using bone meal
- **access-container**: prevents players from accessing most containers
- **access-enderchest**: prevents players from accessing their ender chest
- **enderpearl-from**: prevents ender pearl teleportation out of a region
- **enderpearl-to**: prevents ender pearl teleportation to a region
- **enderman-teleport-from**: prevents enderman from teleporting out of a region
- **enderman-teleport-to**: prevents enderman from teleporting to a region
- **shulker-teleport-from**: prevents shulkers from teleporting out of a region
- **shulker-teleport-to**: prevents shulkers from teleporting to a region
- **item-drop**: prevents players from dropping items
- **item-pickup**: prevents players from picking up items
- **xp-drop-all**: prevents all entities from dropping xp orbs
- **xp-drop-monster**: prevents monsters from dropping xp orbs
- **xp-drop-other**: prevents non-hostile entities from dropping xp orbs
- **level-freeze**: prevents the player levels from increasing/decreasing (xp orbs will still be picked up)
- **xp-freeze**: prevents the player from gaining xp from xp orbs
- **attack-players**: prevents players from damaging other players (PvP)
- **attack-animals**: prevents players from damaging animals
- **attack-villagers**: prevents players from damaging villagers
- **attack-monsters**: prevents players from damaging monsters
- **invincible**: prevents players from taking damage
- **fall-damage**: prevents entities from taking fall damage
- **fall-damage-players**: prevents players from taking fall damage
- **fall-damage-animals**: prevents animals from taking fall damage
- **fall-damage-villagers**: prevents villagers from taking fall damage
- **fall-damage-monsters**: prevents monsters from taking fall damage
- **send-chat**: prevents players from sending chat messages (doesn't block commands)
- **exec-command**: prevents players from executing commands
- **set-spawn**: prevents players from setting their spawn point
- **sleep**: prevents players from sleeping
- **spawn-portal**: prevents creating of portal blocks by lighting obsidian
- **use-portal**: prevents all entities from using portals
- **use-portal-players**: prevents players from using portals
- **use-portal-villagers**: prevents villager entities from using portals
- **use-portal-animals**: prevents animal entities from using portals
- **use-portal-monsters**: prevents monster entities from using portals
- **use-portal-minecarts**: prevents minecart entities from using portals
- **use-portal-items**: prevents item entities from using portals

</details>

# Contribution

Found a bug? Or do you have an idea for a new flag or just general suggestions for the mod?

Don't hesitate to propose them to me. Just hop on to our discord server! Or alternatively, open a
new [issue](https://github.com/Z0rdak/Yet-Another-World-Protector/issues) on our GitHub page!

# Credits

Special thanks to IVEN#2107 for the mod logo!

# License

This mod is released under the GNU LESSER GENERAL PUBLIC LICENSE Version 3.

# Links

* [Yet Another World Protector - Discord](https://discord.gg/X3mjQwTMBV)
* [Yet Another World Protector - Github](https://github.com/Z0rdak/Yet-Another-World-Protector)
* [Yet Another World Protector - Curseforge](https://www.curseforge.com/minecraft/mc-mods/yet-another-world-protector)

# Development roadmap

The following features will be implemented first in the 1.16.5 version, but will also be ported to all future versions. The plan is to have feature parity for all versions. For an overview of work-in-progress features, suggestions and bugs visit the [GitHub project]() board

0. Features from WorldProtector for 1.16.5 and a dedicated Wiki
1. More granular permission system
2. Black- and Whitelist options for all region types
3. [Patchouli](https://github.com/Vazkii/Patchouli/wiki) documentation with a dedicated client-side mod.
4. Region highlighting/rendering
5. ...

# FAQ

- **Q**: Is YAWP available for the Fabric Modloader?
- **A**: No. At this time YAWP has no Fabric port, but if the demand is big enough after the first full release, I will consider it.

- **Q**: Can you implement a flag for \<your feature here\>?
- **A**: Maybe. Consider creating an issue on
  the [YAWG Github](https://github.com/Z0rdak/Yet-Another-World-Protector/issues) page.


- **Q**: Why can't I use the commands provided by YAWP?
- **A**: Make sure you have the required OP level or your UUID set in the configuration.


- **Q**: Why are not all commands with ``/wp <command>`` working and prompting errors?
- **A**: JourneyMap also uses the ``/wp`` command. Try using ``/w-p`` or ``/yawp`` instead.

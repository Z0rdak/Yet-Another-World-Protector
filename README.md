# Yet Another World Protector

[![](http://cf.way2muchnoise.eu/full_663276.svg)![](http://cf.way2muchnoise.eu/versions/663276.svg)](https://www.curseforge.com/minecraft/mc-mods/yawp)

[![](https://img.shields.io/modrinth/dt/py6EMmAJ?logo=modrinth&label=Modrinth)![](https://img.shields.io/modrinth/game-versions/py6EMmAJ?logo=modrinth&label=Latest%20for)](https://modrinth.com/mod/yawp)

[![Discord](https://img.shields.io/discord/1010986742905585759?color=0a48c4&label=discord)](https://discord.gg/d7hArKCUtm)


Yet Another World Protector (YAWP) is _the_ admin tool to protect your minecraft server (dedicated or LAN as well as single-player!).

YAWP is designed to be used on dedicated servers, but can also be used in single player worlds and worlds opened to LAN.  
It allows admins (and players with assigned permission) to protect their creations from various actions/events by
players, mobs and the environment.

Create regions for your builds and apply region flags to protect them.  
Assign players or vanilla teams to regions, so they can manage their own regions.

YAWP was inspired by the forge mod [WorldProtector](https://www.curseforge.com/minecraft/mc-mods/worldprotector) and
the [WorldGuard](https://dev.bukkit.org/projects/worldguard) plugin.

## Supported Versions

This mod is available for Minecraft versions 1.16.5+ for (Neo-) Forge and Fabric.  
Starting with Minecraft **1.20.4**, YAWP has adapted **NeoForge** in favor of Forge.

For detailed [version information](https://z0rdak.github.io/yawp-docs/docs/getting-started/supported-versions) please consult the Wiki or ask on the discord server.

## [Wiki](https://z0rdak.github.io/yawp-docs/) and [Discord](https://discord.gg/d7hArKCUtm)

Please visit the Wiki for detailed documentation on how to use the mod.  
Besides that, it also contains an [FAQ](https://z0rdak.github.io/yawp-docs/docs/getting-started/faq) and a [Getting Started](https://z0rdak.github.io/yawp-docs/docs/getting-started/step-by-step) section.

For further help, feel free to join the [YAWP discord server](https://discord.gg/d7hArKCUtm).

## Contributing

Please refer to [CONTRIBUTING.md](CONTRIBUTING.md) :-)

***

# YAWP API

Currently, YAWP uses cursemaven. To use the YAWP API, add the following snippet to your build.gradle:
```groovy
repositories {
    maven {
        url "https://cursemaven.com"
    }
}

// Note: The cursemaven site explains how to select a specific file: https://www.cursemaven.com/
// The format is "curse.maven:yawp-663276:<fileId>"
dependencies {    
    //Fabric==========    
    modImplementation "curse.maven:yawp-663276:6176022"
    
    //Forge==========
    implementation fg.deobf("curse.maven:yawp-663276:6117986")
    
    //NeoForge
    implementation "curse.maven:yawp-663276:6176016"
}
```

# Links

| [Discord](https://discord.gg/d7hArKCUtm) | [Paypal](https://www.paypal.com/donate/?hosted_button_id=XV65M85SPMD3Y) | [Patreon](https://www.patreon.com/z0rdak) | [Github](https://github.com/Z0rdak/Yet-Another-World-Protector) | [Wiki](https://z0rdak.github.io/yawp-docs/) | [Issues](https://github.com/Z0rdak/Yet-Another-World-Protector/issues) | [Curseforge](https://www.curseforge.com/minecraft/mc-mods/yawp) | [Modrinth](https://modrinth.com/mod/yawp) |
|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|

# License

This mod is released under the GNU AFFERO GENERAL PUBLIC LICENSE Version 3.

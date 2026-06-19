package de.z0rdak.yawp.data;

import net.minecraft.server.MinecraftServer;
import net.minecraft.world.entity.player.Player;

import java.util.UUID;

public final class PlayerManager {

    private PlayerManager() {}

    private static MinecraftServer serverInstance;

    public static void onServerStart(MinecraftServer server) {
        serverInstance = server;
    }

    public static Player getPlayer(UUID uuid){
        return serverInstance.getPlayerList().getPlayer(uuid);
    }

    public static Player getPlayer(String name){
        return serverInstance.getPlayerList().getPlayerByName(name);
    }
}

package de.z0rdak.yawp.handler;

import net.fabricmc.fabric.api.event.lifecycle.v1.*;
import net.fabricmc.fabric.api.networking.v1.PacketSender;
import net.fabricmc.fabric.api.networking.v1.ServerPlayConnectionEvents;
import net.minecraft.entity.Entity;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.network.ServerPlayNetworkHandler;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.world.chunk.WorldChunk;

public final class CommonEvents {

    private CommonEvents() {
    }

    public static void register() {
        ServerTickEvents.END_SERVER_TICK.register(CommonEvents::onServerTick);
        ServerTickEvents.END_WORLD_TICK.register(CommonEvents::onWorldTick);
        ServerChunkEvents.CHUNK_UNLOAD.register(CommonEvents::onChunkUnloaded);
        ServerEntityEvents.ENTITY_LOAD.register(CommonEvents::onEntityAdded);

        ServerLifecycleEvents.SERVER_STOPPED.register(CommonEvents::serverStopping);
        ServerWorldEvents.LOAD.register(CommonEvents::onLoadWorld);
        ServerWorldEvents.UNLOAD.register(CommonEvents::onUnloadWorld);

        ServerPlayConnectionEvents.DISCONNECT.register(CommonEvents::playerLoggedOut);
        ServerPlayConnectionEvents.JOIN.register(CommonEvents::playerLoggedIn);
        ServerPlayConnectionEvents.INIT.register(CommonEvents::playerInit);
    }

    private static void playerInit(ServerPlayNetworkHandler serverPlayNetworkHandler, MinecraftServer minecraftServer) {
    }

    private static void playerLoggedIn(ServerPlayNetworkHandler serverPlayNetworkHandler, PacketSender packetSender, MinecraftServer minecraftServer) {
    }

    private static void playerLoggedOut(ServerPlayNetworkHandler serverPlayNetworkHandler, MinecraftServer minecraftServer) {
    }

    private static void onUnloadWorld(MinecraftServer minecraftServer, ServerWorld serverWorld) {
    }

    private static void onLoadWorld(MinecraftServer minecraftServer, ServerWorld serverWorld) {
    }

    private static void serverStopping(MinecraftServer minecraftServer) {
    }

    private static void onEntityAdded(Entity entity, ServerWorld serverWorld) {
    }

    private static void onWorldTick(ServerWorld serverWorld) {
    }

    private static void onChunkUnloaded(ServerWorld serverWorld, WorldChunk worldChunk) {
    }

    private static void onServerTick(MinecraftServer server) {
    }
}

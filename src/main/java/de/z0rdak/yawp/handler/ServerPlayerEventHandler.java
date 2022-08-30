package de.z0rdak.yawp.handler;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.entity.player.Player;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.server.ServerStartedEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import java.util.List;

import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

/**
 * Eventhandler to track players near regions to handle entering and leaving regions without much overhead.
 */
@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, value = Dist.DEDICATED_SERVER, bus = FORGE)
public class ServerPlayerEventHandler {

    private static MinecraftServer server;

    @SubscribeEvent
    public static void onServerStarted(ServerStartedEvent event){
        server = event.getServer();
    }

    // TODO: configurable to be adjustable for player count
    private static final int checkPlayerMovementInterval = 20;

    public static List<Player> prevPlayerPos;
    public static List<Player> currentPlayerPos;

    @SubscribeEvent
    public static void onServerTick(TickEvent.ServerTickEvent event){
        // just pick start or end it really does not matter

        if (server.getPlayerCount() == 0) {
            return;
        }
        if (event.phase == TickEvent.Phase.END) {
            int scalingPlayerTickInterval = (checkPlayerMovementInterval * server.getPlayerCount());
            if (server.getTickCount() % scalingPlayerTickInterval == 0) {
                /*
                // 1. Get Players in Vincinity of REgions
                // -> List is updated by changing chunk/section events
                if (prevPlayerPos == null) {
                    prevPlayerPos = PlayerTrackingManager.getPlayersInRegionVicinity();
                    currentPlayerPos = new ArrayList<>(prevPlayerPos);
                } else {
                    // Check position on previous tick (cycle) against current position
                    // TODO: check entering & leaving
                    // TODO: tp player to fitting location
                    // TODO: show message for entering & leaving

                    prevPlayerPos = new ArrayList<>(currentPlayerPos);
                }

                 */
            }
        }
    }
}

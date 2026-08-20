package de.z0rdak.yawp.handler;

import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.api.events.region.RegionEvents;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import it.unimi.dsi.fastutil.objects.Object2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.objects.ObjectOpenHashSet;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.Contract;
import org.jspecify.annotations.NonNull;

import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;

import static de.z0rdak.yawp.constants.Constants.MOD_ID;

public final class PlayerPosTracker {

    @Contract(pure = true)
    private PlayerPosTracker() {
        /* This utility class should not be instantiated */
    }

    private static final Logger LOGGER = LogManager.getLogger(MOD_ID.toUpperCase()+ "-PlayerTracker");
    private static final Set<UUID> excludedPlayers = new ObjectOpenHashSet<>();
    private static final Map<UUID, PlayerState> playerStates = new Object2ObjectOpenHashMap<>();

    public static void tickLevel(ServerLevel level) {
        if (RegionIndex.excludes(level))
            return;
        var cache = RegionIndex.getIndexFor(level.dimension());
        updatePlayerPositions(level, cache);
    }

    /** Clears cached state for a player when they disconnect */
    public static void onPlayerDisc(@NonNull ServerPlayer player) {
        UUID playerId = player.getUUID();
        PlayerPosTracker.playerStates.remove(playerId);
        LOGGER.info("Player {} ({}) disconnected. Removing player from cache.", player.getScoreboardName(), playerId);
    }


    private static void updatePlayerPositions(@NonNull ServerLevel level, RegionSpatialIndex cache) {
        var players = level.players();
        for (ServerPlayer player : players) {
            if (excludedPlayers.contains(player.getUUID())) continue;
            handlePlayerMovement(player, cache);
        }
    }

    private static void handlePlayerMovement(@NonNull ServerPlayer player, RegionSpatialIndex cache) {
        BlockPos currentPos = player.blockPosition();
        PlayerState state = playerStates.computeIfAbsent(player.getUUID(), _ -> new PlayerState());
        BlockPos previousPos = state.lastBlockPos;
        if (Objects.equals(previousPos, currentPos))
            return; // no movement

        state.lastBlockPos = currentPos;
        IMarkableRegion previousRegion = state.lastRegion;
        IMarkableRegion currentRegion = cache.getInvolvedRegion(currentPos);

        // since region object identity is stable, != is fine here, else use the UUID
        if (previousRegion != currentRegion) {
            if (previousRegion != null)
                onLeaveRegion(player, previousRegion, previousPos, currentPos);
            if (currentRegion != null)
                onEnterRegion(player, currentRegion, previousPos, currentPos);
            state.lastRegion = currentRegion;
        }
    }

    private static void onEnterRegion(ServerPlayer player, IMarkableRegion region, BlockPos previous, BlockPos current) {
        var enterEvent = new RegionEvent.PlayerEnter(region, player, previous, current);
        RegionEvents.ON_PLAYER_ENTER_REGION.invoke(cb -> cb.onEnter(enterEvent));
    }

    private static void onLeaveRegion(ServerPlayer player, IMarkableRegion region, BlockPos previous, BlockPos current) {
        var leaveEvent = new RegionEvent.PlayerLeave(region, player, previous, current);
        RegionEvents.ON_PLAYER_LEAVE_REGION.invoke(cb -> cb.onLeave(leaveEvent));
    }

    public static void excludePlayer(UUID uuid) {
        excludedPlayers.add(uuid);
    }

    private static class PlayerState {
        private BlockPos lastBlockPos;
        private IMarkableRegion lastRegion;
    }
}
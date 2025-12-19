package de.z0rdak.yawp.handler;

import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.api.events.region.RegionEvents;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import it.unimi.dsi.fastutil.objects.Object2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.objects.ObjectOpenHashSet;
import net.minecraft.core.BlockPos;
import net.minecraft.core.SectionPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.*;

import static de.z0rdak.yawp.constants.Constants.MOD_ID;

public final class PlayerPosTracker {

    private static final Logger LOGGER = LogManager.getLogger(MOD_ID.toUpperCase()+ "-PlayerTracker");
    private static final Set<UUID> excludedPlayers = new ObjectOpenHashSet<>();
    private static final Map<UUID, PlayerState> playerStates = new Object2ObjectOpenHashMap<>();

    public static void tickLevel(ServerLevel level) {
        if (RegionSpatialCache.excludes(level))
            return;
        var cache = RegionSpatialCache.get(level.dimension());
        updatePlayerPositions(level, cache);
    }

    /** Clears cached state for a player when they disconnect */
    public static void onPlayerDisc(ServerPlayer player) {
        UUID playerId = player.getUUID();
        PlayerPosTracker.playerStates.remove(playerId);
        LOGGER.info("Player {} ({}) disconnected. Removing player from cache.", player.getScoreboardName(), playerId);
    }


    private static void updatePlayerPositions(ServerLevel level, RegionSpatialCache cache) {
        var players = level.players();
        for (ServerPlayer player : players) {
            if (excludedPlayers.contains(player.getUUID())) continue;
            handlePlayerMovement(player, cache);
        }
    }

    private static void handlePlayerMovement(ServerPlayer player, RegionSpatialCache cache) {
        BlockPos currentPos = player.blockPosition();
        PlayerState state = playerStates.computeIfAbsent(player.getUUID(), k -> new PlayerState());
        BlockPos previousPos = state.lastBlockPos;

        if (Objects.equals(previousPos, currentPos)) return; // no movement

        state.lastBlockPos = currentPos;

        SectionPos secPos = SectionPos.of(currentPos);
        IMarkableRegion previousRegion = state.lastRegion;
        IMarkableRegion currentRegion = findRegionAt(secPos, currentPos, cache);

        if (!Objects.equals(previousRegion, currentRegion)) {
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


    private static IMarkableRegion findRegionAt(SectionPos section, BlockPos pos, RegionSpatialCache cache) {
        var candidates = cache.getRegionsAtSection(section).stream()
                .filter(IMarkableRegion::isActive)
                .filter(region -> region.contains(pos))
                .toList();
        if (candidates.isEmpty()) return null;
        return Collections.max(candidates, Comparator.comparingInt(IMarkableRegion::getPriority));
    }

    public static void excludePlayer(UUID uuid) {
        excludedPlayers.add(uuid);
    }

    private static class PlayerState {
        private BlockPos lastBlockPos;
        private IMarkableRegion lastRegion;
    }
}
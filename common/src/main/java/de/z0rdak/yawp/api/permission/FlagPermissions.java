package de.z0rdak.yawp.api.permission;

import de.z0rdak.yawp.api.Flag;
import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.api.events.flag.FlagCheckResult;
import de.z0rdak.yawp.core.flag.FlagState;

import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.Nullable;

import java.util.function.Consumer;

import static de.z0rdak.yawp.api.FlagEvaluator.processCheck;

public class FlagPermissions {

    public static FlagState checkFlagPermission(BlockPos pos, Flag flag, ResourceKey<Level> dim) {
        return checkFlagPermission(pos, flag, dim, null, null);
    }

    public static FlagState checkFlagPermission(BlockPos pos, Flag flag, ResourceKey<Level> dim, @Nullable Consumer<FlagCheckResult> onDeny) {
        return checkFlagPermission(pos, flag, dim, null, onDeny);
    }

    public static FlagState checkFlagPermission(BlockPos pos, Flag flag, ResourceKey<Level> dim, @Nullable Consumer<FlagCheckResult> onAllow, @Nullable Consumer<FlagCheckResult> onDeny) {
        FlagCheckRequest checkEvent = new FlagCheckRequest(pos, flag, dim);
        if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
            return FlagState.UNDEFINED;
        }
        return processCheck(checkEvent, onAllow, onDeny);
    }

    public static FlagState checkPlayerFlagPermission(Flag flag, BlockPos pos, ResourceKey<Level> dim, Player player, @Nullable Consumer<FlagCheckResult> onAllow, @Nullable Consumer<FlagCheckResult> onDeny) {
        FlagCheckRequest checkEvent = new FlagCheckRequest(pos, flag, dim, player);
        if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
            return FlagState.UNDEFINED;
        }
        return processCheck(checkEvent, onAllow, onDeny);
    }

    public static FlagState checkPlayerFlagPermission(Flag flag, BlockPos pos, ResourceKey<Level> dim, Player player) {
        return checkPlayerFlagPermission(flag, pos, dim, player, null, null);
    }

    public static FlagState checkPlayerFlagPermission(Flag flag, BlockPos pos, ResourceKey<Level> dim, Player player, @Nullable Consumer<FlagCheckResult> onDeny) {
        return checkPlayerFlagPermission(flag, pos, dim, player, null, onDeny);
    }
}

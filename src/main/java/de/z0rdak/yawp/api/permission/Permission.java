package de.z0rdak.yawp.api.permission;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.commands.CommandSourceType;
import de.z0rdak.yawp.commands.CommandUtil;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Player;

import java.util.List;

import static de.z0rdak.yawp.config.server.CommandPermissionConfig.*;

public class Permission {

    /**
     * Permission needs to be separately handled for
     * - bypassing certain flags
     * - using commands
     * - modifying region properties
     */

    public static List<String> getGroups(IProtectedRegion region, Player player) {
        return CommandUtil.GROUP_LIST;
    }

    public static boolean hasConfigPermAndOpByassFlags(Player player) {
        return hasConfigPermission(player) && byPassFlagAllowed();
    }

    public static boolean hasOwnerPermission(IProtectedRegion region, Player player) {
        return hasGroupPermission(region, player, CommandUtil.OWNER);
    }

    public static boolean hasAnyPermission(IProtectedRegion region, Player player, List<String> groups) {
        return groups.stream()
                .map(group -> hasGroupPermission(region, player, group))
                .reduce(false, (b1, b2) -> b1 || b2);
    }


    public static boolean hasCmdPermission(CommandSourceStack src) {
        CommandSourceType cmdSrcType = CommandSourceType.of(src);
        try {
            return hasConfigPermission(src, cmdSrcType);
        } catch (CommandSyntaxException e) {
            return false;
        }
    }

    public static boolean isAllowedForNonOp(CommandSourceStack src) {
        CommandSourceType cmdSrcType = CommandSourceType.of(src);
        try {
            return hasConfigPermission(src, cmdSrcType) || CommandPermissionConfig.isCmdEnabledForNonOp();
        } catch (CommandSyntaxException e) {
            return false;
        }
    }

    public static boolean hasRequiredOpLevel(Player player) {
        return player.hasPermissions(getRequiredOpLevel());
    }

    public static boolean hasConfigPermission(Player player) {
        return hasUUIDConfigEntry(player) || hasRequiredOpLevel(player);
    }

    public static boolean hasUUIDConfigEntry(Player player) {
        return UUIDsWithPermission().contains(player.getStringUUID());
    }

    public static boolean hasGroupPermission(IProtectedRegion region, Player player, String permissionGroup) {
        return isHierarchyOwnershipEnabled()
                ? hasRegionHierarchyPermission(region, player, permissionGroup)
                : isInGroup(region, player, permissionGroup);
    }

    public static boolean isInGroup(IProtectedRegion region, Player player, String group) {
        return region.isInGroup(player, group);
    }

    private static boolean hasRegionHierarchyPermission(IProtectedRegion region, Player player, String permissionGroup) {
        return hasRegionHierarchyPermission(region, player, permissionGroup, false);
    }

    private static boolean hasRegionHierarchyPermission(IProtectedRegion region, Player player, String permissionGroup, boolean hasPermission) {
        if (region.getParent().equals(region)) {
            return hasPermission || isInGroup(region, player, permissionGroup);
        }
        hasPermission = hasPermission || isInGroup(region, player, permissionGroup);
        return hasRegionHierarchyPermission(region.getParent(), player, permissionGroup, hasPermission);
    }

    public static boolean hasConfigPermission(CommandSourceStack src, CommandSourceType cmdSrcType) throws CommandSyntaxException {
        switch (cmdSrcType) {
            case PLAYER: {
                ServerPlayer player = src.getPlayerOrException();
                return hasConfigPermission(player);
            }
            case COMMAND_BLOCK:
                return isCommandBlockExecutionAllowed();
            case SERVER:
                return true;
        }
        return false;
    }

}

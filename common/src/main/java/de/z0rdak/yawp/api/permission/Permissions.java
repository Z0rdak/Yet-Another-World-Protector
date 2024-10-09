package de.z0rdak.yawp.api.permission;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.commands.CommandSourceType;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.platform.Services;
import de.z0rdak.yawp.platform.services.IPermissionHelper;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Player;

import java.util.Arrays;
import java.util.List;

public final class Permissions {

    public final static String MEMBER = "members";
    public final static String OWNER = "owners";
    public final static List<String> GROUP_LIST = Arrays.asList(MEMBER, OWNER);

    private Permissions(){}
    
    private static final Permissions permission = new Permissions();
    public static Permissions get() {
        return permission;
    }
    /**
     * Permission needs to be separately handled for
     * - bypassing certain flags
     * - using commands
     * - modifying region properties
     */

    public static List<String> getGroups(IProtectedRegion region, Player player) {
        return GROUP_LIST;
    }

    public boolean hasConfigPermAndOpBypassFlags(Player player) {
        boolean byPassFlagAllowed = Services.PERMISSION_CONFIG.byPassFlagAllowed();
        boolean hasConfigPerm = Services.PERMISSION_CONFIG.hasConfigPermission(player);
        return hasConfigPerm && byPassFlagAllowed;
    }
    
    public boolean hasConfigPermission(CommandSourceStack src, CommandSourceType srcType) throws CommandSyntaxException {
        return Services.PERMISSION_CONFIG.hasConfigPermission(src, srcType);
    }

    public boolean hasConfigPermission(Player player) {
        return Services.PERMISSION_CONFIG.hasConfigPermission(player);
    }

    public boolean hasOwnerPermission(IProtectedRegion region, Player player) {
        return hasGroupPermission(region, player, OWNER);
    }

    public boolean hasAnyPermission(IProtectedRegion region, Player player, List<String> groups) {
        return groups.stream()
                .map(group -> hasGroupPermission(region, player, group))
                .reduce(false, (b1, b2) -> b1 || b2);
    }

    public boolean hasCmdPermission(CommandSourceStack src) {
        CommandSourceType cmdSrcType = CommandSourceType.of(src);
        try {
            return Services.PERMISSION_CONFIG.hasConfigPermission(src, cmdSrcType);
        } catch (CommandSyntaxException e) {
            return false;
        }
    }

    public boolean isAllowedForNonOp(CommandSourceStack src) {
        CommandSourceType cmdSrcType = CommandSourceType.of(src);
        try {
            boolean hasConfigPerm = Services.PERMISSION_CONFIG.hasConfigPermission(src, cmdSrcType);
            return hasConfigPerm || Services.PERMISSION_CONFIG.isCmdEnabledForNonOp();
        } catch (CommandSyntaxException e) {
            return false;
        }
    }

    public boolean hasGroupPermission(IProtectedRegion region, Player player, String permissionGroup) {
        return Services.PERMISSION_CONFIG.isHierarchyOwnershipEnabled()
                ? hasRegionHierarchyPermission(region, player, permissionGroup)
                : isInGroup(region, player, permissionGroup);
    }

    public boolean isInGroup(IProtectedRegion region, Player player, String group) {
        return region.isInGroup(player, group);
    }

    private boolean hasRegionHierarchyPermission(IProtectedRegion region, Player player, String permissionGroup) {
        return hasRegionHierarchyPermission(region, player, permissionGroup, false);
    }

    private boolean hasRegionHierarchyPermission(IProtectedRegion region, Player player, String permissionGroup, boolean hasPermission) {
        if (region.getParent().equals(region)) {
            return hasPermission || isInGroup(region, player, permissionGroup);
        }
        hasPermission = hasPermission || isInGroup(region, player, permissionGroup);
        return hasRegionHierarchyPermission(region.getParent(), player, permissionGroup, hasPermission);
    }

}

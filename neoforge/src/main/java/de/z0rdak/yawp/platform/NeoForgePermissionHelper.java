package de.z0rdak.yawp.platform;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.api.permission.Permissions;
import de.z0rdak.yawp.commands.CommandSourceType;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.platform.services.IPermissionHelper;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.world.entity.player.Player;

import java.util.List;

public class NeoForgePermissionHelper implements IPermissionHelper {

    @Override
    public boolean hasConfigPermission(CommandSourceStack src, CommandSourceType srcType) throws CommandSyntaxException {
        return Permissions.get().hasConfigPermission(src, srcType);
    }

    @Override
    public boolean isAllowedForNonOp(CommandSourceStack src) {
        return Permissions.get().isAllowedForNonOp(src);
    }

    @Override
    public boolean hasConfigPermAndOpBypassFlags(Player player) {
        return Permissions.get().hasConfigPermAndOpBypassFlags(player);
    }

    @Override
    public boolean hasOwnerPermission(IProtectedRegion region, Player player) {
        return Permissions.get().hasOwnerPermission(region, player);
    }

    @Override
    public boolean hasAnyPermission(IProtectedRegion region, Player player, List<String> groups) {
        return Permissions.get().hasAnyPermission(region, player, groups);
    }

    @Override
    public boolean hasCmdPermission(CommandSourceStack src) {
        return Permissions.get().hasCmdPermission(src);
    }

    @Override
    public boolean hasGroupPermission(IProtectedRegion region, Player player, String permissionGroup) {
        return Permissions.get().hasGroupPermission(region, player, permissionGroup);
    }

    @Override
    public boolean isInGroup(IProtectedRegion region, Player player, String group) {
        return Permissions.get().isInGroup(region, player, group);
    }
}
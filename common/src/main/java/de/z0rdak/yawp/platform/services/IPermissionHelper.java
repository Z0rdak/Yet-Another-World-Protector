package de.z0rdak.yawp.platform.services;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.commands.CommandSourceType;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.world.entity.player.Player;

import java.util.List;
import java.util.Set;

public interface IPermissionHelper {
    
    boolean hasConfigPermission(CommandSourceStack src, CommandSourceType srcType) throws CommandSyntaxException;
    boolean isAllowedForNonOp(CommandSourceStack src);
    boolean hasConfigPermAndOpBypassFlags(Player player);
    boolean hasOwnerPermission(IProtectedRegion region, Player player);
    boolean hasAnyPermission(IProtectedRegion region, Player player, List<String> groups);
    boolean hasCmdPermission(CommandSourceStack src);
    boolean hasGroupPermission(IProtectedRegion region, Player player, String permissionGroup);
    boolean isInGroup(IProtectedRegion region, Player player, String group);
    //boolean hasRegionHierarchyPermission(IProtectedRegion region, Player player, String permissionGroup);
    //boolean hasRegionHierarchyPermission(IProtectedRegion region, Player player, String permissionGroup, boolean hasPermission);    
    
}

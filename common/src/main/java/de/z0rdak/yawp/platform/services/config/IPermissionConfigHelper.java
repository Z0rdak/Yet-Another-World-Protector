package de.z0rdak.yawp.platform.services.config;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.commands.CommandSourceType;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.world.entity.player.Player;

import java.util.Set;

public interface IPermissionConfigHelper {
    
    boolean isCommandBlockExecutionAllowed();
    boolean isHierarchyOwnershipEnabled();
    Set<String> UUIDsWithPermission();
    boolean isCmdEnabledForNonOp();
    int getRequiredOpLevel();
    boolean byPassFlagAllowed();
    boolean hasConfigPermission(Player player);
    boolean hasConfigPermission(CommandSourceStack src, CommandSourceType srcType) throws CommandSyntaxException;
    boolean isReadOnlyAllowed();
    boolean allowRegionTp();
    boolean isMarkerCreationEnabled();
}

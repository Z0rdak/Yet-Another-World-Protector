package de.z0rdak.yawp.platform.config;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.commands.CommandSourceType;
import de.z0rdak.yawp.config.server.PermissionConfig;
import de.z0rdak.yawp.platform.services.config.IPermissionConfigHelper;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.world.entity.player.Player;

import java.util.Set;

public class ForgePermissionConfigHelper implements IPermissionConfigHelper {

    @Override
    public boolean isCommandBlockExecutionAllowed() {
        return PermissionConfig.isCommandBlockExecutionAllowed();
    }

    @Override
    public boolean isHierarchyOwnershipEnabled() {
        return PermissionConfig.isHierarchyOwnershipEnabled();
    }

    @Override
    public Set<String> UUIDsWithPermission() {
        return PermissionConfig.UUIDsWithPermission();
    }

    @Override
    public boolean isCmdEnabledForNonOp() {
        return PermissionConfig.isCmdEnabledForNonOp();
    }

    @Override
    public int getRequiredOpLevel() {
        return PermissionConfig.getRequiredOpLevel();
    }

    @Override
    public boolean byPassFlagAllowed() {
        return PermissionConfig.byPassFlagAllowed();
    }

    @Override
    public boolean hasConfigPermission(Player player) {
        return PermissionConfig.hasConfigPermission(player);
    }

    @Override
    public boolean hasConfigPermission(CommandSourceStack src, CommandSourceType srcType) throws CommandSyntaxException {
        return PermissionConfig.hasConfigPermission(src, srcType);
    }

    @Override
    public boolean isReadOnlyAllowed() {
        return PermissionConfig.isReadOnlyAllowed();
    }

    @Override
    public boolean allowRegionTp() {
        return PermissionConfig.allowRegionTp();
    }
    
    @Override
    public boolean isMarkerCreationEnabled() {
        return PermissionConfig.isMarkerCreationEnabled();
    }
}
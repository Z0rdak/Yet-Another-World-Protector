package de.z0rdak.yawp.util.visualization;

import de.z0rdak.yawp.core.area.BlockDisplayProperties;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;

public class RegionOutlineBuilder {

    public static Entity createBlockDisplay(ServerLevel level, BlockPos pos, CompoundTag displayTag) {
        return EntityType.loadEntityRecursive(displayTag, level, p_396566_ -> {
            p_396566_.moveTo(pos.getX(), pos.getY(), pos.getZ(), p_396566_.getYRot(), p_396566_.getXRot());
            return p_396566_;
        });
    }

    public static CompoundTag buildBlockDisplayTag(BlockDisplayProperties properties) {
        var blockDisplayTag = new CompoundTag();
        String displayBlockId = EntityType.BLOCK_DISPLAY.builtInRegistryHolder().key().location().toString();
        blockDisplayTag.putString("id", displayBlockId);
        blockDisplayTag.putBoolean("Glowing", properties.hasGlow());
        var blockstateTag = new CompoundTag();
        blockstateTag.putString("Name", properties.blockRl().toString());
        blockDisplayTag.put("block_state", blockstateTag);
        var brightnessTag = new CompoundTag();
        brightnessTag.putInt("sky", properties.lightLevel());
        brightnessTag.putInt("block", properties.lightLevel());
        blockDisplayTag.put("brightness", brightnessTag);
        return blockDisplayTag;
    }
}

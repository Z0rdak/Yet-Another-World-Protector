package de.z0rdak.yawp.util.visualization;

import de.z0rdak.yawp.core.area.MarkedArea;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntitySpawnReason;
import net.minecraft.world.entity.EntityType;

import java.util.Set;
import java.util.stream.Collectors;

public class RegionOutlineBuilder {

    public static Set<Entity> buildAreaDisplaySet(ServerLevel level, Set<BlockPos> frame, CompoundTag blockDisplayTag) {
        return frame.stream()
                .map(blockPos -> buildBlockDisplay(level, blockPos, blockDisplayTag))
                .collect(Collectors.toSet());
    }

    public static Entity buildBlockDisplay(ServerLevel level, BlockPos pos, CompoundTag displayTag) {
        displayTag.putString("id",  EntityType.BLOCK_DISPLAY.builtInRegistryHolder().key().location().toString());
        return EntityType.loadEntityRecursive(displayTag, level, EntitySpawnReason.COMMAND, p_396566_ -> {
            p_396566_.snapTo(pos.getX(), pos.getY(), pos.getZ(), p_396566_.getYRot(), p_396566_.getXRot());
            return p_396566_;
        });
    }

    public static CompoundTag buildBlockDisplayTag(BlockDisplayProperty properties) {
        var blockDisplayTag = new CompoundTag();
        blockDisplayTag.putBoolean("Glowing", properties.glowing());
        blockDisplayTag.putBoolean("PersistenceRequired", properties.persistent());
        var blockstateTag = new CompoundTag();
        blockstateTag.putString("Name", properties.blockName());
        blockDisplayTag.put("block_state", blockstateTag);
        return blockDisplayTag;
    }
}

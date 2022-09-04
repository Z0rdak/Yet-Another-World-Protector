package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.area.CylinderArea;
import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;

public final class CylinderRegion extends AbstractMarkableRegion {

    public CylinderRegion(CompoundTag nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public CylinderRegion(String name, CylinderArea area, Player owner, ResourceKey<Level> dimension) {
        this(name, area, new BlockPos(area.getCenter().x, area.getCenter().y, area.getCenter().z), owner, dimension);
    }

    public CylinderRegion(String name, CylinderArea area, BlockPos tpPos, Player owner, ResourceKey<Level> dimension) {
        super(name, area, tpPos, owner, dimension);
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        this.area = new CylinderArea(nbt.getCompound(RegionNBT.AREA));
    }
}

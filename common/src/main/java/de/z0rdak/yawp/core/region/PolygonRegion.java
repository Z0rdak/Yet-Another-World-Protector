package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.constants.serialization.RegionNbtKeys;
import de.z0rdak.yawp.core.area.Polygon3DArea;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

public final class PolygonRegion extends AbstractMarkableRegion {

    public PolygonRegion(CompoundTag nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public PolygonRegion(String name, Polygon3DArea area, Player owner, ResourceKey<Level> dimension) {
        super(name, area, owner, dimension);
    }

    public PolygonRegion(String name, Polygon3DArea area, BlockPos tpTarget, Player owner, ResourceKey<Level> dimension) {
        super(name, area, tpTarget, owner, dimension);
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        this.area = new Polygon3DArea(nbt.getCompound(RegionNbtKeys.AREA));
    }
}

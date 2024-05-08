package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

public final class SphereRegion extends AbstractMarkableRegion {

    public SphereRegion(CompoundTag nbt) {
        super(nbt);
        this.deserializeNBT(provider, nbt);
    }

    public SphereRegion(String name, SphereArea area, Player player, ResourceKey<Level> dimension) {
        super(name, area, new BlockPos(area.getCenter().getX(), area.getCenter().getY(), area.getCenter().getZ()), player, dimension);
    }

    public SphereRegion(String name, SphereArea area, BlockPos tpPos, Player player, ResourceKey<Level> dimension) {
        super(name, area, tpPos, player, dimension);
    }

    @Override
    public void deserializeNBT(HolderLookup.Provider provider, CompoundTag nbt) {
        super.deserializeNBT(provider, nbt);
        this.area = new SphereArea(nbt.getCompound(RegionNBT.AREA));
    }
}

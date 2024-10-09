package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.constants.serialization.RegionNbtKeys;
import de.z0rdak.yawp.core.area.SphereArea;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

public final class SphereRegion extends AbstractMarkableRegion {

    public SphereRegion(CompoundTag nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public SphereRegion(String name, SphereArea area, ResourceKey<Level> dimension) {
        this(name, area, area.getCenterPos(), null, dimension);
    }

    public SphereRegion(String name, SphereArea area, Player owner, ResourceKey<Level> dimension) {
        super(name, area, area.getCenterPos(), owner, dimension);
    }

    public SphereRegion(String name, SphereArea area, BlockPos tpPos, Player player, ResourceKey<Level> dimension) {
        super(name, area, tpPos, player, dimension);
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        this.area = new SphereArea(nbt.getCompound(RegionNbtKeys.AREA));
    }
}

package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

public class SphereRegion extends AbstractMarkableRegion {

    public SphereRegion(CompoundTag nbt) {
        this.deserializeNBT(nbt);
    }

    public SphereRegion(String name, SphereArea area, Player player, ResourceKey<Level> dimension) {
        super(name, area, player, dimension);
    }

    public SphereRegion(String name, SphereArea area, BlockPos tpPos, Player player, ResourceKey<Level> dimension) {
        super(name, area, tpPos, player, dimension);
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        this.area = new SphereArea(nbt.getCompound(RegionNBT.AREA));
    }
}

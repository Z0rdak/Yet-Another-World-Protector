package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.registry.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public final class SphereRegion extends AbstractMarkableRegion {

    public SphereRegion(NbtCompound nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public SphereRegion(String name, SphereArea area, PlayerEntity player, RegistryKey<World> dimension) {
        super(name, area, new BlockPos(area.getCenter().getX(), area.getCenter().getY(), area.getCenter().getZ()), player, dimension);
    }

    public SphereRegion(String name, SphereArea area, BlockPos tpPos, PlayerEntity player, RegistryKey<World> dimension) {
        super(name, area, tpPos, player, dimension);
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        super.deserializeNBT(nbt);
        this.area = new SphereArea(nbt.getCompound(RegionNBT.AREA));
    }
}

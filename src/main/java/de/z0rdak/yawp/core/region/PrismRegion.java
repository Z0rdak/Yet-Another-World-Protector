package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.area.PrismArea;
import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.util.math.BlockPos;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.registry.RegistryKey;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.world.World;

public final class PrismRegion extends AbstractMarkableRegion {

    public PrismRegion(NbtCompound nbt){
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public PrismRegion(String name, PrismArea area, PlayerEntity owner, RegistryKey<World> dimension) {
        super(name, area, owner, dimension);
    }

    public PrismRegion(String name, PrismArea area, BlockPos tpTarget, PlayerEntity owner, RegistryKey<World> dimension) {
        super(name, area, tpTarget, owner, dimension);
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        super.deserializeNBT(nbt);
        this.area = new PrismArea(nbt.getCompound(RegionNBT.AREA));
    }
}

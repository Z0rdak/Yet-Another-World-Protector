package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.area.PrismArea;
import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

public final class PrismRegion extends AbstractMarkableRegion {

    public PrismRegion(CompoundTag nbt){
        super(nbt);
        this.deserializeNBT(provider, nbt);
    }

    public PrismRegion(String name, PrismArea area, Player owner, ResourceKey<Level> dimension) {
        super(name, area, owner, dimension);
    }

    public PrismRegion(String name, PrismArea area, BlockPos tpTarget, Player owner, ResourceKey<Level> dimension) {
        super(name, area, tpTarget, owner, dimension);
    }

    @Override
    public void deserializeNBT(HolderLookup.Provider provider, CompoundTag nbt) {
        super.deserializeNBT(provider, nbt);
        this.area = new PrismArea(nbt.getCompound(RegionNBT.AREA));
    }
}

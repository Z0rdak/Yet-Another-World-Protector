package de.z0rdak.regionshield.server.data;

import de.z0rdak.regionshield.common.core.region.CuboidRegion;
import de.z0rdak.regionshield.common.core.region.DimensionalRegion;
import de.z0rdak.regionshield.common.core.region.IMarkableRegion;
import de.z0rdak.regionshield.common.util.constants.RegionNBT;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.RegistryKey;
import net.minecraft.world.World;
import net.minecraftforge.common.util.INBTSerializable;

import java.util.*;

public class DimensionalRegionCache extends HashMap<String, IMarkableRegion> implements INBTSerializable<CompoundNBT> {

    private DimensionalRegion dimensionalRegion;

    public DimensionalRegionCache(DimensionalRegion region) {
        this();
        this.dimensionalRegion = region;
    }

    public DimensionalRegionCache(RegistryKey<World> dim) {
        super();
        this.dimensionalRegion = new DimensionalRegion(dim);
    }

    private DimensionalRegionCache() {
        super();
    }

    public DimensionalRegionCache(CompoundNBT nbt) {
        this();
        deserializeNBT(nbt);
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        CompoundNBT regions = new CompoundNBT();
        for (Entry<String, IMarkableRegion> regionEntry : this.entrySet()) {
            regions.put(regionEntry.getKey(), regionEntry.getValue().serializeNBT());
        }
        nbt.put("regions", regions);
        nbt.put(RegionNBT.DIM_REGION, this.dimensionalRegion.serializeNBT());
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        CompoundNBT dimensionalRegion = nbt.getCompound(RegionNBT.DIM_REGION);
        this.dimensionalRegion = new DimensionalRegion(dimensionalRegion);
        CompoundNBT regions = nbt.getCompound("regions");
        for (String regionKey : regions.getAllKeys()) {
            CompoundNBT regionNbt = regions.getCompound(regionKey);
            CuboidRegion region = new CuboidRegion(regionNbt);
            this.addRegion(region);
        }
    }

    private void addRegion(CuboidRegion region) {
        this.put(region.getName(), region);
    }

    public Collection<String> getRegionNames() {
        return null;
    }

    public void clearRegions() {
        this.clear();
    }
}

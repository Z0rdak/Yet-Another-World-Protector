package de.z0rdak.regionshield.managers.data.region;

import de.z0rdak.regionshield.RegionShield;
import de.z0rdak.regionshield.core.area.AreaType;
import de.z0rdak.regionshield.core.region.*;
import de.z0rdak.regionshield.util.constants.NBTConstants;
import de.z0rdak.regionshield.util.constants.RegionNBT;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.RegistryKey;
import net.minecraft.world.World;
import net.minecraft.world.storage.WorldSavedData;
import net.minecraftforge.fml.common.Mod;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

@Mod.EventBusSubscriber(modid = RegionShield.MODID)
public class DimensionRegionCache extends WorldSavedData {

    public Map<String, IMarkableRegion> regionsInDimension;
    private DimensionalRegion dimensionalRegion;

    public DimensionRegionCache(RegistryKey<World> dim){
        this(new DimensionalRegion(dim));
    }

    public DimensionRegionCache(String dataName){
        super(getDataName(dataName));
        this.regionsInDimension = new HashMap<>();
        this.dimensionalRegion = new DimensionalRegion(dataName);
    }

    public DimensionRegionCache(DimensionalRegion dimensionalRegion){
        super(getDataName(dimensionalRegion));
        this.dimensionalRegion = dimensionalRegion;
        this.regionsInDimension = new HashMap<>();
    }

    private static String getDataName(DimensionalRegion dim){
        return getDataName(dim.getName());
    }

    public static String getDataName(String dim){
        return RegionShield.MODID_SHORT + "-" + dim.replace(':', '-');
    }

    @Override
    public void load(CompoundNBT nbt) {
        RegionShield.LOGGER.debug("##################");
        RegionShield.LOGGER.debug("Loading region data");
        RegionShield.LOGGER.debug("##################");
        CompoundNBT regionsNbt = nbt.getCompound(NBTConstants.REGIONS);
        CompoundNBT dimRegionNbt = nbt.getCompound(NBTConstants.DIM_REGION);
        this.dimensionalRegion = new DimensionalRegion(dimRegionNbt);
        this.regionsInDimension = new HashMap<>();
        regionsNbt.getAllKeys().forEach( regionName -> {
            CompoundNBT regionNbt = regionsNbt.getCompound(regionName);
            AreaType areaType = AreaType.of(regionNbt.getString(RegionNBT.AREA_TYPE));
            switch (areaType) {
                case CUBOID:
                    regionsInDimension.put(regionName, new CuboidRegion(regionNbt));
                    break;
                case SPHERE:
                    regionsInDimension.put(regionName, new SphericalRegion(regionNbt));
                    break;
                case CYLINDER:
                    break;
                case POLYGON_3D:
                    break;
                case PRISM:
                    break;
                case UNKNOWN:
                    break;
                default:
                    break;
            }
        });
    }

    @Override
    public CompoundNBT save(CompoundNBT nbt) {
        RegionShield.LOGGER.debug("##################");
        RegionShield.LOGGER.debug("Saving region data");
        RegionShield.LOGGER.debug("##################");
        nbt.put(NBTConstants.DIM_REGION, this.dimensionalRegion.serializeNBT());
        CompoundNBT regions = new CompoundNBT();
        this.regionsInDimension.forEach( (name, region) -> {
            regions.put(name, region.serializeNBT());
        });
        nbt.put(NBTConstants.REGIONS, regions);
        return nbt;
    }

    public DimensionalRegion getDimensionalRegion() {
        return dimensionalRegion;
    }

    public void addRegion(IMarkableRegion region) {
        this.regionsInDimension.put(region.getName(), region);
        this.setDirty();
    }

    public Collection<String> getRegionNames() {
        return regionsInDimension.keySet();
    }

    public void removeRegion(String regionName){
        if (this.contains(regionName)){
            this.regionsInDimension.remove(regionName);
        }
    }

    public void clearRegions() {
        this.regionsInDimension.clear();
    }

    public boolean contains(String regionName) {
        return regionsInDimension.containsKey(regionName);
    }

    public IMarkableRegion get(String regionName) {
        return regionsInDimension.get(regionName);
    }
}

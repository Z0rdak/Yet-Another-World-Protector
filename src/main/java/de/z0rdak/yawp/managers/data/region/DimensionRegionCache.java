package de.z0rdak.yawp.managers.data.region;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.util.constants.NBTConstants;
import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.scoreboard.Team;
import net.minecraft.util.RegistryKey;
import net.minecraft.world.World;
import net.minecraft.world.storage.WorldSavedData;
import net.minecraftforge.fml.common.Mod;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID)
public class DimensionRegionCache extends WorldSavedData {

    public Map<String, IMarkableRegion> regionsInDimension;
    private DimensionalRegion dimensionalRegion;

    public DimensionRegionCache(RegistryKey<World> dim){
        this(new DimensionalRegion(dim));
    }

    public DimensionRegionCache(String dimKey){
        super(getDataName(dimKey));
        this.regionsInDimension = new HashMap<>();
        this.dimensionalRegion = new DimensionalRegion(dimKey);
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
        return YetAnotherWorldProtector.MODID + "-" + dim.replace(':', '-');
    }

    @Override
    public void load(CompoundNBT nbt) {
        CompoundNBT regionsNbt = nbt.getCompound(NBTConstants.REGIONS);
        CompoundNBT dimRegionNbt = nbt.getCompound(NBTConstants.DIM_REGION);
        this.dimensionalRegion = new DimensionalRegion(dimRegionNbt);
        YetAnotherWorldProtector.LOGGER.debug("Loading dim data for '" + this.dimensionalRegion.getName() +"'");
        this.regionsInDimension = new HashMap<>();
        regionsNbt.getAllKeys().forEach( regionName -> {
            CompoundNBT regionNbt = regionsNbt.getCompound(regionName);
            AreaType areaType = AreaType.of(regionNbt.getString(RegionNBT.AREA_TYPE));
            switch (areaType) {
                case CUBOID:
                    regionsInDimension.put(regionName, new CuboidRegion(regionNbt));
                    break;
                case SPHERE:
                    regionsInDimension.put(regionName, new SphereRegion(regionNbt));
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
        YetAnotherWorldProtector.LOGGER.debug("Saving dim data for '" + this.dimensionalRegion.getName() +"'");
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

    public void addOwner(ServerPlayerEntity player) {
        this.dimensionalRegion.addOwner(player);
        this.setDirty();
    }

    public void addOwner(Team team) {
        this.dimensionalRegion.addOwner(team);
        this.setDirty();
    }

    public void addMember(ServerPlayerEntity player) {
        this.dimensionalRegion.addMember(player);
        this.setDirty();
    }

    public void addMember(Team team) {
        this.dimensionalRegion.addMember(team);
        this.setDirty();
    }

    public void removeOwner(ServerPlayerEntity player) {
        this.dimensionalRegion.removeOwner(player);
        this.setDirty();
    }

    public void removeOwner(Team team) {
        this.dimensionalRegion.removeOwner(team);
        this.setDirty();
    }

    public void removeMember(ServerPlayerEntity player) {
        this.dimensionalRegion.removeMember(player);
        this.setDirty();
    }

    public void removeMember(Team team) {
        this.dimensionalRegion.removeMember(team);
        this.setDirty();
    }

    public void addFlag(IFlag flag){
        this.dimensionalRegion.addFlag(flag);
        this.setDirty();
    }

    public void removeFlag(String flag){
        this.dimensionalRegion.removeFlag(flag);
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

    public Collection<String> getDimFlags(){
        return this.dimensionalRegion.getFlags()
                .stream()
                .map(IFlag::getFlagName)
                .collect(Collectors.toList());
    }
}

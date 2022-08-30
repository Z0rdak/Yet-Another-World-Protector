package de.z0rdak.yawp.managers.data.region;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.affiliation.PlayerContainer;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.util.constants.NBTConstants;
import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.scores.Team;
import net.minecraftforge.common.util.INBTSerializable;
import net.minecraftforge.fml.common.Mod;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID)
public class DimensionRegionCache implements INBTSerializable<CompoundTag> {

    public Map<String, IMarkableRegion> regionsInDimension;
    private DimensionalRegion dimensionalRegion;

    public DimensionRegionCache(ResourceKey<Level> dim){
        this(new DimensionalRegion(dim));
    }

    public DimensionRegionCache(CompoundTag nbt){
        this.deserializeNBT(nbt);
    }

    public DimensionRegionCache(DimensionalRegion dimensionalRegion){
        this.dimensionalRegion = dimensionalRegion;
        this.regionsInDimension = new HashMap<>();
    }

    private static String getDataName(DimensionalRegion dim){
        return getDataName(dim.getName());
    }

    public static String getDataName(String dim){
        return YetAnotherWorldProtector.MODID + "-" + dim.replace(':', '-');
    }

    public void load(CompoundTag nbt) {
        CompoundTag regionsNbt = nbt.getCompound(NBTConstants.REGIONS);
        CompoundTag dimRegionNbt = nbt.getCompound(NBTConstants.DIM_REGION);
        this.dimensionalRegion = new DimensionalRegion(dimRegionNbt);
        YetAnotherWorldProtector.LOGGER.info("Loading dim data for '" + this.dimensionalRegion.getName() +"'");
        this.regionsInDimension = new HashMap<>();
        regionsNbt.getAllKeys().forEach( regionName -> {
            CompoundTag regionNbt = regionsNbt.getCompound(regionName);
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

    public CompoundTag save(CompoundTag nbt) {
        YetAnotherWorldProtector.LOGGER.info("Saving dim data for '" + this.dimensionalRegion.getName() +"'");
        nbt.put(NBTConstants.DIM_REGION, this.dimensionalRegion.serializeNBT());
        CompoundTag regions = new CompoundTag();
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
        RegionDataManager.save();
    }

    public void addOwner(ServerPlayer player) {
        this.dimensionalRegion.addOwner(player);
        RegionDataManager.save();
    }

    public void addOwner(Team team) {
        this.dimensionalRegion.addOwner(team);
        RegionDataManager.save();
    }

    public void addMember(ServerPlayer player) {
        this.dimensionalRegion.addMember(player);
        RegionDataManager.save();
    }

    public void addMember(Team team) {
        this.dimensionalRegion.addMember(team);
        RegionDataManager.save();
    }

    public void removeOwner(ServerPlayer player) {
        this.dimensionalRegion.removeOwner(player);
        RegionDataManager.save();
    }

    public void removeOwner(Team team) {
        this.dimensionalRegion.removeOwner(team);
        RegionDataManager.save();
    }

    public void removeMember(ServerPlayer player) {
        this.dimensionalRegion.removeMember(player);
        RegionDataManager.save();
    }

    public void setDimState(boolean active){
        this.dimensionalRegion.setIsActive(active);
        RegionDataManager.save();
    }

    public void removeMember(Team team) {
        this.dimensionalRegion.removeMember(team);
        RegionDataManager.save();
    }

    public void addFlag(IFlag flag){
        this.dimensionalRegion.addFlag(flag);
        RegionDataManager.save();
    }

    public void removeFlag(String flag){
        this.dimensionalRegion.removeFlag(flag);
        RegionDataManager.save();
    }

    public Collection<String> getRegionNames() {
        return regionsInDimension.keySet();
    }

    public void removeRegion(String regionName){
        if (this.contains(regionName)){
            this.regionsInDimension.remove(regionName);
            RegionDataManager.save();
        }
    }

    public void clearRegions() {
        this.regionsInDimension.clear();
        RegionDataManager.save();
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

    @Override
    public CompoundTag serializeNBT() {
        return this.save(new CompoundTag());
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        this.load(nbt);
    }

    public boolean hasOwner(Player player) {
        PlayerContainer owners = this.dimensionalRegion.getOwners();
        return owners.containsPlayer(player.getUUID())
                || (player.getTeam() != null && owners.containsTeam(player.getTeam()));
    }

    public boolean hasMember(Player player) {
        PlayerContainer members = this.dimensionalRegion.getMembers();
        return members.containsPlayer(player.getUUID())
                || (player.getTeam() != null && members.containsTeam(player.getTeam()));
    }
}

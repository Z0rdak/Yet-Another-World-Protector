package de.z0rdak.yawp.managers.data.region;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.affiliation.PlayerContainer;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.util.constants.NBTConstants;
import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.scoreboard.Team;
import net.minecraft.util.RegistryKey;
import net.minecraft.world.World;
import net.minecraftforge.common.util.INBTSerializable;
import net.minecraftforge.fml.common.Mod;

import javax.annotation.Nullable;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID)
public class DimensionRegionCache implements INBTSerializable<CompoundNBT> {

    public Map<String, IMarkableRegion> regionsInDimension;
    private DimensionalRegion dimensionalRegion;

    public DimensionRegionCache(RegistryKey<World> dim){
        this(new DimensionalRegion(dim));
    }

    public DimensionRegionCache(CompoundNBT nbt){
        this.deserializeNBT(nbt);
    }

    public DimensionRegionCache(DimensionalRegion dimensionalRegion){
        this.dimensionalRegion = dimensionalRegion;
        this.regionsInDimension = new HashMap<>();
    }

    public RegistryKey<World> dimensionKey(){
        return this.dimensionalRegion.getDimensionKey();
    }

    private static String getDataName(DimensionalRegion dim){
        return getDataName(dim.getName());
    }

    public static String getDataName(String dim){
        return YetAnotherWorldProtector.MODID + "-" + dim.replace(':', '-');
    }

    public void load(CompoundNBT nbt) {
        CompoundNBT regionsNbt = nbt.getCompound(NBTConstants.REGIONS);
        CompoundNBT dimRegionNbt = nbt.getCompound(NBTConstants.DIM_REGION);
        this.dimensionalRegion = new DimensionalRegion(dimRegionNbt);
        YetAnotherWorldProtector.LOGGER.info("Loading dim data for '" + this.dimensionalRegion.getName() +"'");
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
                default:
                    break;
            }
        });
    }

    public CompoundNBT save(CompoundNBT nbt) {
        YetAnotherWorldProtector.LOGGER.info("Saving dim data for '" + this.dimensionalRegion.getName() +"'");
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
        RegionDataManager.save();
    }

    public void addOwner(ServerPlayerEntity player) {
        this.dimensionalRegion.addOwner(player);
        RegionDataManager.save();
    }

    public void addOwner(Team team) {
        this.dimensionalRegion.addOwner(team);
        RegionDataManager.save();
    }

    public void addMember(ServerPlayerEntity player) {
        this.dimensionalRegion.addMember(player);
        RegionDataManager.save();
    }

    public void addMember(Team team) {
        this.dimensionalRegion.addMember(team);
        RegionDataManager.save();
    }

    public void removeOwner(ServerPlayerEntity player) {
        this.dimensionalRegion.removeOwner(player);
        RegionDataManager.save();
    }

    public void removeOwner(Team team) {
        this.dimensionalRegion.removeOwner(team);
        RegionDataManager.save();
    }

    public void removeMember(ServerPlayerEntity player) {
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

    @Nullable
    public IMarkableRegion getRegion(String regionName) {
        return this.regionsInDimension.get(regionName);
    }

    public Collection<String> getRegionNames() {
        return regionsInDimension.keySet();
    }

    public Collection<IMarkableRegion> getRegions() {
        return regionsInDimension.values();
    }

    public void removeRegion(String regionName){
        if (this.contains(regionName)){
            this.regionsInDimension.remove(regionName);
            RegionDataManager.save();
        }
    }

    public void removeRegion(IMarkableRegion region){
        if (this.contains(region.getName())){
            this.regionsInDimension.remove(region.getName());
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
                .map(IFlag::getFlagIdentifier)
                .collect(Collectors.toList());
    }

    @Override
    public CompoundNBT serializeNBT() {
        return this.save(new CompoundNBT());
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        this.load(nbt);
    }

    public boolean hasOwner(PlayerEntity player) {
        PlayerContainer owners = this.dimensionalRegion.getOwners();
        return owners.containsPlayer(player.getUUID())
                || (player.getTeam() != null && owners.containsTeam(player.getTeam()));
    }

    public boolean hasMember(PlayerEntity player) {
        PlayerContainer members = this.dimensionalRegion.getMembers();
        return members.containsPlayer(player.getUUID())
                || (player.getTeam() != null && members.containsTeam(player.getTeam()));
    }
}

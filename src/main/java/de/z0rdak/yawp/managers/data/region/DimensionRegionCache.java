package de.z0rdak.yawp.managers.data.region;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.affiliation.PlayerContainer;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.*;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.scoreboard.Team;
import net.minecraft.util.RegistryKey;
import net.minecraft.world.World;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.common.util.INBTSerializable;
import net.minecraftforge.fml.common.Mod;

import javax.annotation.Nullable;
import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.util.constants.RegionNBT.*;

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
        return this.dimensionalRegion.getDim();
    }

    private static String getDataName(DimensionalRegion dim){
        return getDataName(dim.getName());
    }

    public static String getDataName(String dim){
        return YetAnotherWorldProtector.MODID + "-" + dim.replace(':', '-');
    }

    public DimensionalRegion getDimensionalRegion() {
        return dimensionalRegion;
    }

    public void addRegion(IMarkableRegion region) {
        this.regionsInDimension.put(region.getName(), region);
        this.dimensionalRegion.addChild(region);

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

    public Set<String> getDimFlagNames(){
        return this.dimensionalRegion.getFlags()
                .stream()
                .map(IFlag::getFlagIdentifier)
                .collect(Collectors.toSet());
    }

    public List<IFlag> getDimFlags(){
        return new ArrayList<>(this.dimensionalRegion.getFlags());
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        nbt.put(DIM_REGION, this.dimensionalRegion.serializeNBT());
        CompoundNBT regions = new CompoundNBT();
        this.regionsInDimension.forEach( (name, region) -> {
            regions.put(name, region.serializeNBT());
        });
        nbt.put(REGIONS, regions);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        if (nbt.contains(DIM_REGION, Constants.NBT.TAG_COMPOUND)) {
            this.dimensionalRegion = new DimensionalRegion(nbt.getCompound(DIM_REGION));
        } else {
            // TODO: Add dimKey as property to nbt compound to init new default dimensional region
            throw new IllegalArgumentException("Unable to load dimensional region data from NBT");
        }
        this.regionsInDimension = new HashMap<>();
        YetAnotherWorldProtector.LOGGER.debug("Loading dim data for '" + this.dimensionalRegion.getDim().location() +"'");
        CompoundNBT regionsNbt = nbt.getCompound(REGIONS);
        regionsNbt.getAllKeys().forEach(regionName -> {
            CompoundNBT regionNbt = regionsNbt.getCompound(regionName);
            AreaType areaType = AreaType.of(regionNbt.getString(AREA_TYPE));
            if (areaType != null) {
                YetAnotherWorldProtector.LOGGER.debug("Loading region data for region '" + regionName + "'");
                IMarkableRegion newRegion = DimensionRegionCache.deserializeLocalRegion(areaType, regionNbt);
                this.addRegion(newRegion);
            } else {
                YetAnotherWorldProtector.LOGGER.error("Unable to read region type for region '" + regionName + "'!");
            }
        });
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

    public static IProtectedRegion deserializeRegion(RegionType regionType, CompoundNBT regionNbt) {
        switch (regionType) {
            case GLOBAL:
                throw new UnsupportedOperationException("Global not supported yet");
            case DIMENSION:
                return new DimensionalRegion(regionNbt);
            case LOCAL:
                AreaType areaType = AreaType.of(regionNbt.getString(AREA_TYPE));
                if (areaType == null) {
                    YetAnotherWorldProtector.LOGGER.error("Unable to read region type for region!");
                    return null;
                } else {
                    return deserializeLocalRegion(areaType, regionNbt);
                }
            case TEMPLATE:
                throw new UnsupportedOperationException("Template not supported yet");
            default:
                throw new IllegalArgumentException("");
        }
    }

    public static IMarkableRegion deserializeLocalRegion(AreaType areaType, CompoundNBT regionNbt) {
        switch (areaType) {
            case CUBOID:
                return new CuboidRegion(regionNbt);
            case CYLINDER:
                return new CylinderRegion(regionNbt);
            case SPHERE:
                return new SphereRegion(regionNbt);
            case POLYGON_3D:
                return new PolygonRegion(regionNbt);
            case PRISM:
                return new PrismRegion(regionNbt);
            default:
                throw new IllegalArgumentException("Unable to read area type of region '" + regionNbt.getString(NAME) + "'!");
        }
    }
}

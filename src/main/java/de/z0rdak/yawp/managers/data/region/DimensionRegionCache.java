package de.z0rdak.yawp.managers.data.region;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.INbtSerializable;
import de.z0rdak.yawp.core.affiliation.PlayerContainer;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.*;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtElement;
import net.minecraft.scoreboard.Team;
import net.minecraft.server.network.ServerPlayerEntity;
import net.minecraft.util.registry.RegistryKey;
import net.minecraft.world.World;

import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.util.constants.RegionNBT.*;

public class DimensionRegionCache implements INbtSerializable<NbtCompound> {

    private Map<String, IMarkableRegion> regionsInDimension;
    private DimensionalRegion dimensionalRegion;

    public DimensionRegionCache(RegistryKey<World> dim) {
        this(new DimensionalRegion(dim));
    }

    public DimensionRegionCache(NbtCompound nbt) {
        this.deserializeNBT(nbt);
    }

    public DimensionRegionCache(DimensionalRegion dimensionalRegion) {
        this.dimensionalRegion = dimensionalRegion;
        this.regionsInDimension = new HashMap<>();
    }

    public Map<String, IMarkableRegion> getRegionsInDimension() {
        return Collections.unmodifiableMap(regionsInDimension);
    }

    private static String getDataName(DimensionalRegion dim) {
        return getDataName(dim.getName());
    }

    public static String getDataName(String dim) {
        return YetAnotherWorldProtector.MODID + "-" + dim.replace(':', '-');
    }

    public static IMarkableRegion deserializeLocalRegion(AreaType areaType, NbtCompound regionNbt) {
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

    public DimensionalRegion getDimensionalRegion() {
        return dimensionalRegion;
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

    public void setDimState(boolean active) {
        this.dimensionalRegion.setIsActive(active);
        RegionDataManager.save();
    }

    public void addFlag(IFlag flag) {
        this.dimensionalRegion.addFlag(flag);
        RegionDataManager.save();
    }

    public void removeFlag(String flag) {
        this.dimensionalRegion.removeFlag(flag);
        RegionDataManager.save();
    }

    public IMarkableRegion getRegion(String regionName) {
        return this.regionsInDimension.get(regionName);
    }

    public Collection<String> getRegionNames() {
        return regionsInDimension.keySet();
    }

    public Collection<IMarkableRegion> getRegions() {
        return regionsInDimension.values();
    }

    public void removeRegion(IMarkableRegion region) {
        if (this.contains(region.getName())) {
            this.regionsInDimension.remove(region.getName());
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

    public RegistryKey<World> dimensionKey() {
        return this.dimensionalRegion.getDim();
    }

    public void addRegion(IMarkableRegion region) {
        this.dimensionalRegion.addChild(region);
        this.regionsInDimension.put(region.getName(), region);
    }

    public Set<String> getDimFlagNames() {
        return this.dimensionalRegion.getFlags()
                .stream()
                .map(IFlag::getFlagIdentifier)
                .collect(Collectors.toSet());
    }

    public List<IFlag> getDimFlags() {
        return new ArrayList<>(this.dimensionalRegion.getFlags());
    }

    public boolean hasOwner(PlayerEntity player) {
        PlayerContainer owners = this.dimensionalRegion.getOwners();
        return owners.containsPlayer(player.getUuid())
                || (player.getScoreboardTeam() != null && owners.containsTeam((Team) player.getScoreboardTeam()));
    }

    public boolean hasMember(PlayerEntity player) {
        PlayerContainer members = this.dimensionalRegion.getMembers();
        return members.containsPlayer(player.getUuid())
                || (player.getScoreboardTeam() != null && members.containsTeam((Team) player.getScoreboardTeam()));
    }

    @Override
    public NbtCompound serializeNBT() {
        NbtCompound nbt = new NbtCompound();
        nbt.put(DIM_REGION, this.dimensionalRegion.serializeNBT());
        NbtCompound regions = new NbtCompound();
        this.regionsInDimension.forEach((name, region) -> {
            regions.put(name, region.serializeNBT());
        });
        nbt.put(REGIONS, regions);
        return nbt;
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        if (nbt.contains(DIM_REGION, NbtElement.COMPOUND_TYPE)) {
            this.dimensionalRegion = new DimensionalRegion(nbt.getCompound(DIM_REGION));
        } else {
            // TODO: Add dimKey as property to nbt compound to init new default dimensional region
            throw new IllegalArgumentException("Unable to load dimensional region data from NBT");
        }
        this.regionsInDimension = new HashMap<>();
        NbtCompound regionsNbt = nbt.getCompound(REGIONS);
        regionsNbt.getKeys().forEach(regionName -> {
            NbtCompound regionNbt = regionsNbt.getCompound(regionName);
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
}

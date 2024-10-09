package de.z0rdak.yawp.data.region;

import de.z0rdak.yawp.api.permission.Permissions;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.INbtSerializable;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.group.PlayerContainer;
import de.z0rdak.yawp.core.region.*;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.constants.serialization.RegionNbtKeys.*;

public class DimensionRegionCache implements INbtSerializable<CompoundTag> {

    private Map<String, IMarkableRegion> regionsInDimension;
    private DimensionalRegion dimensionalRegion;

    public DimensionRegionCache(ResourceKey<Level> dim) {
        this(new DimensionalRegion(dim, RegionDataManager.get().getGlobalRegion()));
    }

    public DimensionRegionCache(CompoundTag nbt) {
        this.deserializeNBT(nbt);
    }

    protected DimensionRegionCache(DimensionalRegion dimensionalRegion) {
        this.dimensionalRegion = dimensionalRegion;
        this.regionsInDimension = new HashMap<>();
    }

    private static String getDataName(DimensionalRegion dim) {
        return getDataName(dim.getName());
    }

    private static String getDataName(String dim) {
        return Constants.MOD_ID + "-" + dim.replace(':', '-');
    }

    private static IMarkableRegion deserializeLocalRegion(AreaType areaType, CompoundTag regionNbt) {
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

    public ResourceKey<Level> dimensionKey() {
        return this.dimensionalRegion.getDim();
    }

    public DimensionalRegion getDimensionalRegion() {
        return dimensionalRegion;
    }

    public Map<String, IMarkableRegion> getRegionsInDimension() {
        return Collections.unmodifiableMap(regionsInDimension);
    }

    public Collection<IMarkableRegion> getAllLocal() {
        return regionsInDimension.values().stream().collect(Collectors.toUnmodifiableList());
    }

    public void addRegion(IProtectedRegion parent, IMarkableRegion child) {
        parent.addChild(child);
        this.regionsInDimension.put(child.getName(), child);
    }

    public void addRegion(IMarkableRegion child) {
        this.dimensionalRegion.addChild(child);
        this.regionsInDimension.put(child.getName(), child);
    }

    public int getRegionCount() {
        return regionsInDimension.size();
    }

    public Collection<String> getRegionNames() {
        return new ArrayList<>(regionsInDimension.keySet());
    }

    @Nullable
    public IMarkableRegion getRegion(String regionName) {
        return this.regionsInDimension.get(regionName);
    }

    public void removeRegion(IMarkableRegion region) {
        if (this.contains(region.getName())) {
            this.regionsInDimension.remove(region.getName());
            if (region.getParent().getRegionType() == RegionType.DIMENSION) {
                region.getParent().removeChild(region);
            }
        }
    }

    public void clearRegions() {
        this.regionsInDimension.clear();
        this.dimensionalRegion.clearChildren();
    }

    public boolean contains(String regionName) {
        return regionsInDimension.containsKey(regionName);
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = new CompoundTag();
        nbt.put(DIM_REGION, this.dimensionalRegion.serializeNBT());
        CompoundTag regions = new CompoundTag();
        this.regionsInDimension.forEach((name, region) -> {
            regions.put(name, region.serializeNBT());
        });
        nbt.put(REGIONS, regions);
        return nbt;
    }

    public void renameRegion(IMarkableRegion region, String regionName) {
        if (this.regionsInDimension.containsKey(regionName)) {
            throw new IllegalArgumentException("Region with name '" + regionName + "' already exists in dimension '" + this.dimensionalRegion.getName() + "'!");
        }
        IMarkableRegion currentRegion = this.regionsInDimension.get(region.getName());
        IProtectedRegion parent = currentRegion.getParent();
        this.removeRegion(currentRegion);
        currentRegion.rename(regionName);
        this.addRegion(parent, currentRegion);
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        if (nbt.contains(DIM_REGION, Tag.TAG_COMPOUND)) {
            this.dimensionalRegion = new DimensionalRegion(nbt.getCompound(DIM_REGION));
        } else {
            throw new IllegalArgumentException("Unable to load dimensional region data from NBT");
        }
        this.regionsInDimension = new HashMap<>();
        CompoundTag regionsNbt = nbt.getCompound(REGIONS);
        regionsNbt.getAllKeys().forEach(regionName -> {
            CompoundTag regionNbt = regionsNbt.getCompound(regionName);
            AreaType areaType = AreaType.of(regionNbt.getString(AREA_TYPE));
            if (areaType != null) {
                Constants.LOGGER.debug("Loading region data for region '{}'", regionName);
                IMarkableRegion newRegion = DimensionRegionCache.deserializeLocalRegion(areaType, regionNbt);
                this.addRegion(this.getDimensionalRegion(), newRegion);
            } else {
                Constants.LOGGER.error("Unable to read region type for region '{}'!", regionName);
            }
        });
    }

    public boolean hasOwner(Player player) {
        PlayerContainer owners = this.dimensionalRegion.getGroup(Permissions.OWNER);
        return owners.hasPlayer(player.getUUID())
                || (player.getTeam() != null && owners.hasTeam(player.getTeam().getName()));
    }

    public boolean hasMember(Player player) {
        PlayerContainer members = this.dimensionalRegion.getGroup(Permissions.MEMBER);
        return members.hasPlayer(player.getUUID())
                || (player.getTeam() != null && members.hasTeam(player.getTeam().getName()));
    }
}

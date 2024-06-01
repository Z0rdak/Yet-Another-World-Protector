package de.z0rdak.yawp.managers.data.region;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.CommandUtil;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.group.PlayerContainer;
import de.z0rdak.yawp.core.region.*;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.neoforged.neoforge.common.util.INBTSerializable;

import javax.annotation.Nullable;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static de.z0rdak.yawp.util.constants.RegionNBT.*;

public class DimensionRegionCache implements INBTSerializable<CompoundTag> {

    private Map<String, IMarkableRegion> regionsInDimension;
    private DimensionalRegion dimensionalRegion;

    public DimensionRegionCache(ResourceKey<Level> dim) {
        this(new DimensionalRegion(dim, RegionDataManager.get().getGlobalRegion()));
    }

    public DimensionRegionCache(HolderLookup.Provider provider, CompoundTag nbt) {
        this.deserializeNBT(provider, nbt);
    }

    public DimensionRegionCache(DimensionalRegion dimensionalRegion) {
        this.dimensionalRegion = dimensionalRegion;
        this.regionsInDimension = new HashMap<>();
    }

    private static String getDataName(DimensionalRegion dim) {
        return getDataName(dim.getName());
    }

    public static String getDataName(String dim) {
        return YetAnotherWorldProtector.MODID + "-" + dim.replace(':', '-');
    }

    public ResourceKey<Level> dimensionKey() {
        return this.dimensionalRegion.getDim();
    }

    public Map<String, IMarkableRegion> getRegionsInDimension() {
        return Collections.unmodifiableMap(regionsInDimension);
    }

    public DimensionalRegion getDimensionalRegion() {
        return dimensionalRegion;
    }

    public void addRegion(IProtectedRegion parent, IMarkableRegion child) {
        parent.addChild(child);
        this.regionsInDimension.put(child.getName(), child);
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

    public boolean contains(String regionName) {
        return regionsInDimension.containsKey(regionName);
    }

    public IMarkableRegion get(String regionName) {
        return regionsInDimension.get(regionName);
    }

    public static IMarkableRegion deserializeLocalRegion(HolderLookup.Provider provider, AreaType areaType, CompoundTag regionNbt) {
        switch (areaType) {
            case CUBOID:
                return new CuboidRegion(provider, regionNbt);
            case CYLINDER:
                return new CylinderRegion(provider, regionNbt);
            case SPHERE:
                return new SphereRegion(provider, regionNbt);
            case POLYGON_3D:
                return new PolygonRegion(provider, regionNbt);
            case PRISM:
                return new PrismRegion(provider, regionNbt);
            default:
                throw new IllegalArgumentException("Unable to read area type of region '" + regionNbt.getString(NAME) + "'!");
        }
    }

    @Override
    public CompoundTag serializeNBT(HolderLookup.Provider provider) {
        CompoundTag nbt = new CompoundTag();
        nbt.put(DIM_REGION, this.dimensionalRegion.serializeNBT(provider));
        CompoundTag regions = new CompoundTag();
        this.regionsInDimension.forEach((name, region) -> {
            regions.put(name, region.serializeNBT(provider));
        });
        nbt.put(REGIONS, regions);
        return nbt;
    }

    public boolean hasOwner(Player player) {
        PlayerContainer owners = this.dimensionalRegion.getGroup(CommandUtil.OWNER);
        return owners.hasPlayer(player.getUUID())
                || (player.getTeam() != null && owners.hasTeam(player.getTeam().getName()));
    }

    public boolean hasMember(Player player) {
        PlayerContainer members = this.dimensionalRegion.getGroup(CommandUtil.MEMBER);
        return members.hasPlayer(player.getUUID())
                || (player.getTeam() != null && members.hasTeam(player.getTeam().getName()));
    }

    @Override
    public void deserializeNBT(HolderLookup.Provider provider, CompoundTag nbt) {
        if (nbt.contains(DIM_REGION, Tag.TAG_COMPOUND)) {
            this.dimensionalRegion = new DimensionalRegion(provider, nbt.getCompound(DIM_REGION));
        } else {
            throw new IllegalArgumentException("Unable to load dimensional region data from NBT");
        }
        this.regionsInDimension = new HashMap<>();
        CompoundTag regionsNbt = nbt.getCompound(REGIONS);
        regionsNbt.getAllKeys().forEach(regionName -> {
            CompoundTag regionNbt = regionsNbt.getCompound(regionName);
            AreaType areaType = AreaType.of(regionNbt.getString(AREA_TYPE));
            if (areaType != null) {
                YetAnotherWorldProtector.LOGGER.debug("Loading region data for region '" + regionName + "'");
                IMarkableRegion newRegion = DimensionRegionCache.deserializeLocalRegion(provider, areaType, regionNbt);
                this.addRegion(this.getDimensionalRegion(), newRegion);
            } else {
                YetAnotherWorldProtector.LOGGER.error("Unable to read region type for region '" + regionName + "'!");
            }
        });
    }

}

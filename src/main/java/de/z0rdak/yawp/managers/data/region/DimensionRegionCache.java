package de.z0rdak.yawp.managers.data.region;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.affiliation.PlayerContainer;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.*;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.ResourceKey;

import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

import net.minecraftforge.common.util.INBTSerializable;
import net.minecraftforge.fml.common.Mod;

import javax.annotation.Nullable;
import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.util.constants.RegionNBT.*;

@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID)
public class DimensionRegionCache implements INBTSerializable<CompoundTag> {

    private Map<String, IMarkableRegion> regionsInDimension;
    private DimensionalRegion dimensionalRegion;

    public DimensionRegionCache(ResourceKey<Level> dim) {
        this(new DimensionalRegion(dim));
    }

    public DimensionRegionCache(CompoundTag nbt) {
        this.deserializeNBT(nbt);
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

    public void addRegion(IMarkableRegion region) {
        this.dimensionalRegion.addChild(region);
        this.regionsInDimension.put(region.getName(), region);
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
        }
    }


    public void renameRegion(IMarkableRegion region, String regionName) {
        IMarkableRegion currentRegion = this.regionsInDimension.get(region.getName());
        // TODO: Rename me -> remove region, clone region, change name, add region, restore hierarchy
        // TODO: update children name in parent
        IProtectedRegion parent = currentRegion.getParent();
        this.regionsInDimension.put(regionName, currentRegion);
    }

    public boolean contains(String regionName) {
        return regionsInDimension.containsKey(regionName);
    }

    public IMarkableRegion get(String regionName) {
        return regionsInDimension.get(regionName);
    }

    public Set<String> getDimFlagNames() {
        return this.dimensionalRegion.getFlags()
                .stream()
                .map(IFlag::getName)
                .collect(Collectors.toSet());
    }

    public List<IFlag> getDimFlags() {
        return new ArrayList<>(this.dimensionalRegion.getFlags());
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

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        if (nbt.contains(DIM_REGION, Tag.TAG_COMPOUND)) {
            this.dimensionalRegion = new DimensionalRegion(nbt.getCompound(DIM_REGION));
        } else {
            // TODO: Add dimKey as property to nbt compound to init new default dimensional region
            throw new IllegalArgumentException("Unable to load dimensional region data from NBT");
        }
        this.regionsInDimension = new HashMap<>();
        CompoundTag regionsNbt = nbt.getCompound(REGIONS);
        regionsNbt.getAllKeys().forEach(regionName -> {
            CompoundTag regionNbt = regionsNbt.getCompound(regionName);
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

    public static IMarkableRegion deserializeLocalRegion(AreaType areaType, CompoundTag regionNbt) {
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

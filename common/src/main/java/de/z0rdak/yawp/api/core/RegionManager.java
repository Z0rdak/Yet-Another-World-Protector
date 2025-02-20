package de.z0rdak.yawp.api.core;

import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.region.GlobalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.data.region.DimensionRegionCache;
import de.z0rdak.yawp.data.region.RegionDataManager;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Vec3i;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.levelgen.structure.BoundingBox;

import java.util.*;
import java.util.stream.Collectors;


public final class RegionManager implements IRegionManager {

    private final RegionDataManager rdm;

    private RegionManager() {
        this.rdm = RegionDataManager.get();
    }

    public static RegionManager get() {
        return new RegionManager();
    }

    @Override
    public GlobalRegion getGlobalRegion() {
        return this.rdm.getGlobalRegion();
    }

    @Override
    public void resetGlobal() {
        this.rdm.resetGlobalRegion();
        save();
    }

    @Override
    public Optional<IProtectedRegion> getDimensionalRegion(ResourceKey<Level> dim) {
        Optional<DimensionRegionCache> cache = rdm.getCache(dim);
        return cache.map(DimensionRegionCache::getDimensionalRegion);
    }

    @Override
    public Optional<DimensionRegionCache> getDimensionCache(ResourceKey<Level> dim) {
        return rdm.getCache(dim);
    }

    @Override
    public void save() {
        rdm.setDirty();
    }

    /**
     * Gets the DimensionalRegion API for the specified dimension key.
     *
     * @param dim the dimension key to get the API for
     * @return the DimensionalRegionApi for the specified dimension key if it exists, otherwise Optional.Empty
     */
    @Override
    public Optional<IDimensionRegionApi> getDimRegionApi(ResourceKey<Level> dim) {
        if (rdm.containsCacheFor(dim))
            return Optional.of(new DimensionRegionApi(dim));
        return Optional.empty();
    }

    /**
     * Gets the DimensionalRegion API for the specified dimension key (E.g. "minecraft:overworld").
     *
     * @param dimKey the dimension key to get the API for
     * @return the DimensionalRegionApi for the specified dimension key if it exists, otherwise Optional.Empty
     */
    @Override
    public Optional<IDimensionRegionApi> getDimRegionApiByKey(String dimKey) {
        return this.getDimRegionApi(getDimApiKey(dimKey));
    }

    @Override
    public ResourceKey<Level> getDimApiKey(String dimKey) {
        return ResourceKey.create(Registries.DIMENSION, ResourceLocation.parse(dimKey));
    }

    @Override
    public boolean hasRegionFor(ResourceKey<Level> dim) {
        return rdm.containsCacheFor(dim);
    }

    @Override
    public boolean createDimRegion(ResourceKey<Level> dim) {
        if (hasRegionFor(dim)) {
            return false;
        }
        rdm.newCacheFor(dim);
        save();
        return true;
    }

    @Override
    public Set<ResourceKey<Level>> getDimensions() {
        return rdm.getDimKeys();
    }

    @Override
    public void resetDimension(ResourceKey<Level> dim) {
        rdm.resetDimensionCache(dim);
        save();
    }

    public static class DimensionRegionApi implements IDimensionRegionApi {

        private final RegionDataManager rdm;
        private final ResourceKey<Level> dim;
        private final DimensionRegionCache cache;

        private DimensionRegionApi(ResourceKey<Level> dim) {
            this.rdm = RegionDataManager.get();
            this.dim = dim;
            this.cache = this.rdm.cacheFor(dim);
        }

        @Override
        public void save() {
            rdm.setDirty();
        }

        @Override
        public Optional<IMarkableRegion> getLocalRegion(String name) {
            IMarkableRegion region = cache.getRegion(name);
            return region != null ? Optional.of(region) : Optional.empty();
        }

        @Override
        public ResourceKey<Level> getDimKey() {
            return dim;
        }

        @Override
        public boolean hasLocal(String name) {
            return cache.contains(name);
        }

        @Override
        public boolean addLocalRegion(IMarkableRegion region) {
            if (hasLocal(region.getName())) return false;
            cache.addRegion(region);
            return true;
        }

        @Override
        public boolean removeLocal(IMarkableRegion region) {
            return removeLocalRegion(region.getName());
        }

        @Override
        public boolean removeLocalRegion(String regionName) {
            if (hasLocal(regionName)) {
                Optional<IMarkableRegion> localRegion = getLocalRegion(regionName);
                if (localRegion.isPresent()) {
                    cache.removeRegion(localRegion.get());
                    return true;
                }
                return false;
            }
            return false;
        }

        @Override
        public boolean hasRegionAt(BlockPos pos) {
            return !getRegionsAt(pos).isEmpty();
        }

        @Override
        public List<IMarkableRegion> getRegionsAt(BlockPos pos) {
            return getAllLocalRegions().stream()
                    .filter(region -> region.contains(pos))
                    .collect(Collectors.toList());
        }

        @Override
        public Collection<IMarkableRegion> getAllLocalRegions() {
            return this.cache.getAllLocal();
        }

        @Override
        public List<IMarkableRegion> getRegionsIn(Vec3i pos1, Vec3i pos2) {
            return getRegionsInBox(BoundingBox.fromCorners(pos1, pos2));
        }

        @Override
        public List<IMarkableRegion> getRegionsInCoords(int x1, int y1, int z1, int x2, int y2, int z2) {
            return getRegionsIn(new BlockPos(x1, y1, z1), new BlockPos(x2, y2, z2));
        }

        @Override
        public List<IMarkableRegion> getRegionsInBox(BoundingBox blockBox) {
            CuboidArea predicateArea = new CuboidArea(blockBox);
            return getAllLocalRegions().stream()
                    .filter(r -> predicateArea.containsOther(r.getArea()))
                    .toList();
        }

        @Override
        public List<IMarkableRegion> getIntersectingRegions(BoundingBox blockBox) {
            CuboidArea predicateArea = new CuboidArea(blockBox);
            return getAllLocalRegions().stream()
                    .filter(r -> predicateArea.intersects(r.getArea()))
                    .collect(Collectors.toList());
        }

        @Override
        public List<IMarkableRegion> getIntersectingRegions(IMarkableRegion region) {
            return getAllLocalRegions().stream()
                    .filter(r -> !r.equals(region)) // filter input region from the result
                    .filter(r -> r.getArea().intersects(region.getArea()))
                    .collect(Collectors.toList());
        }

        @Override
        public List<IMarkableRegion> getContainingRegions(IMarkableRegion region) {
            return getAllLocalRegions().stream()
                    .filter(r -> !r.equals(region)) // filter input region from the result
                    .filter(r -> r.getArea().containsOther(region.getArea()))
                    .collect(Collectors.toList());
        }

        @Override
        public List<IMarkableRegion> getContainedRegions(IMarkableRegion region) {
            return getAllLocalRegions().stream()
                    .filter(r -> !r.equals(region)) // filter input region from the result
                    .filter(r -> region.getArea().containsOther(r.getArea()))
                    .collect(Collectors.toList());
        }

        /**
         * Gets the region with the highest priority among all involved regions at the given location and dimension. <br>
         * This considers the active state of the region as well. <br>
         *
         * @param position the position to check for involved regions
         * @return the region with the highest priority among all involved regions which contain the given location
         */
        @Override
        public Optional<IMarkableRegion> getInvolvedRegionFor(BlockPos position) {
            List<IMarkableRegion> regionsForPos = getInvolvedRegionsFor(position);
            if (regionsForPos.isEmpty()) {
                return Optional.empty();
            } else {
                return Optional.of(Collections.max(regionsForPos, Comparator.comparing(IMarkableRegion::getPriority)));
            }
        }

        /**
         * Gets all active regions which contain the provided position in the  <br>
         *
         * @param pos the position to check for involved regions
         * @return all active regions which contain the given location and dimension
         */
        private List<IMarkableRegion> getInvolvedRegionsFor(BlockPos pos) {
            return getRegionsAt(pos).stream()
                    .filter(IMarkableRegion::isActive)
                    .collect(Collectors.toList());
        }

        @Override
        public Optional<IProtectedRegion> findResponsibleRegion(BlockPos pos) {
            Optional<IMarkableRegion> maybeRegion = getInvolvedRegionFor(pos);
            if (maybeRegion.isEmpty()) {
                IProtectedRegion dimRegion = cache.getDimensionalRegion();
                if (dimRegion.isActive()) {
                    return Optional.of(dimRegion);
                } else {
                    return rdm.getGlobalRegion().isActive()
                            ? Optional.of(rdm.getGlobalRegion())
                            : Optional.empty();
                }
            }
            IProtectedRegion region = maybeRegion.get();
            return Optional.of(region);
        }

    }

}
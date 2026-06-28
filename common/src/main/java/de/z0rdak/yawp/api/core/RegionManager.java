package de.z0rdak.yawp.api.core;

import de.z0rdak.yawp.api.core.region.hierarchy.RegionHierarchy;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.region.GlobalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.data.region.LevelData;
import de.z0rdak.yawp.data.region.RegionDataManager;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Vec3i;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.levelgen.structure.BoundingBox;

import java.util.*;
import java.util.stream.Collectors;


public final class RegionManager implements IRegionManager {

    private RegionManager() {}

    public static RegionManager get() {
        return new RegionManager();
    }

    public static RegionType getRegionType(Identifier regionId){


        throw new IllegalArgumentException();
    }

    @Override
    public GlobalRegion getGlobalRegion() {
        return RegionDataManager.getGlobalRegion();
    }

    @Override
    public void resetGlobal() {
        RegionDataManager.getGlobalRegionData().reset();
        saveAll();
    }

    @Override
    public Optional<IProtectedRegion> getDimensionalRegion(ResourceKey<Level> dim) {
        Optional<LevelData> cache = getLevelRegionData(dim);
        return cache.map(LevelData::getDim);
    }

    @Override
    public Optional<LevelData> getLevelRegionData(Identifier levelRl) {
        return RegionDataManager.getLevelRegionData(levelRl);
    }


    @Override
    public Optional<IMarkableRegion> getLocalRegion(Identifier regionId) {
        return Optional.empty();
    }


    @Override
    public Optional<IProtectedRegion> getRegion(Identifier regionId) {

        return Optional.empty();
    }

    @Override
    public Optional<LevelData> getLevelRegionData(ResourceKey<Level> dim) {
        return RegionDataManager.getLevelRegionData(dim);
    }

    @Override
    public List<LevelData> getLevelRegionData() {
        return RegionDataManager.getAllLevelRegionData();
    }

    @Override
    public void saveAll() {
        RegionDataManager.save();
    }

    @Override
    public void save(ServerLevel level) {
        RegionDataManager.saveLevel(level);
    }

    @Override
    public void save(IProtectedRegion region) {
        if (region.getRegionType() == RegionType.GLOBAL) {
            RegionDataManager.saveGlobalData();
        } else {
            save(region.getDim());
        }
    }

    @Override
    public void save(ResourceKey<Level> levelRl) {
        RegionDataManager.saveLevel(levelRl.identifier());
    }

    /**
     * Gets the DimensionalRegion API for the specified dimension key.
     *
     * @param dim the dimension key to get the API for
     * @return the DimensionalRegionApi for the specified dimension key if it exists, otherwise Optional.Empty
     */
    @Override
    public Optional<ILevelRegionApi> getDimRegionApi(ResourceKey<Level> dim) {
        if (RegionDataManager.hasLevel(dim.identifier())){
            var maybeLevelRegionData= RegionDataManager.getLevelRegionData(dim.identifier());
            if (maybeLevelRegionData.isPresent()) {
                return Optional.of(new DimensionRegionApi(maybeLevelRegionData.get()));
            }
        }
        return Optional.empty();
    }

    /**
     * Gets the DimensionalRegion API for the specified dimension key (E.g. "minecraft:overworld").
     *
     * @param dimKey the dimension key to get the API for
     * @return the DimensionalRegionApi for the specified dimension key if it exists, otherwise Optional.Empty
     */
    @Override
    public Optional<ILevelRegionApi> getDimRegionApiByKey(String dimKey) {
        return this.getDimRegionApi(getDimApiKey(dimKey));
    }

    @Override
    public ResourceKey<Level> getDimApiKey(String dimKey) {
        return ResourceKey.create(Registries.DIMENSION, Identifier.parse(dimKey));
    }

    @Override
    public boolean hasLevelData(Identifier levelRl) {
        return RegionDataManager.hasLevel(levelRl);
    }
    @Override
    public boolean hasLevelData(ResourceKey<Level> dim) {
        return RegionDataManager.hasLevel(dim.identifier());
    }

    @Override
    public LevelData trackLevel(ResourceKey<Level> dim) {
        return RegionDataManager.addTrackingFor(dim.identifier());
    }

    @Override
    public void untrackLevel(ResourceKey<Level> dim) {
        RegionDataManager.removeTrackingFor(dim.identifier());
    }

    @Override
    public Set<Identifier> getLevels() {
        return RegionDataManager.getLevels();
    }

    public Set<String> getLevelNames() {
        return RegionDataManager.getLevelNames();
    }

    @Override
    public void resetLevelData(ResourceKey<Level> level) {
        RegionDataManager.resetLevelData(level);
        save(level);
    }

    public static class DimensionRegionApi implements ILevelRegionApi {
        private final LevelData levelData;

        private DimensionRegionApi(LevelData levelData) {
            this.levelData = levelData;
        }

        @Override
        public void save() {
            RegionManager.get().saveAll();
        }

        @Override
        public Optional<IMarkableRegion> getLocalRegion(String name) {
            IMarkableRegion region = levelData.getLocal(name);
            return region != null ? Optional.of(region) : Optional.empty();
        }

        @Override
        public ResourceKey<Level> getDimKey() {
            return levelData.getDim().getDim();
        }

        @Override
        public LevelData getCache() {
            return levelData;
        }

        @Override
        public boolean hasLocal(String name) {
            return levelData.hasLocal(name);
        }

        @Override
        public boolean addLocalRegion(IMarkableRegion region) {
            if (hasLocal(region.getName())) return false;
            RegionHierarchy.setParent(region, levelData.getDim());
            levelData.addLocal(region);
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
                    levelData.removeLocal(localRegion.get());
                    return true;
                }
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
            return this.levelData.getLocalList();
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
        public List<IMarkableRegion> getRegionsAround(BlockPos pos, int radius) {
            SphereArea predicateArea = new SphereArea(pos, radius);
            return getAllLocalRegions().stream()
                    .filter(r -> predicateArea.intersects(r.getArea()))
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
                IProtectedRegion dimRegion = levelData.getDim();
                if (dimRegion.isActive()) {
                    return Optional.of(dimRegion);
                } else {
                    return RegionManager.get().getGlobalRegion().isActive()
                            ? Optional.of(RegionManager.get().getGlobalRegion())
                            : Optional.empty();
                }
            }
            IProtectedRegion region = maybeRegion.get();
            return Optional.of(region);
        }
    }

}
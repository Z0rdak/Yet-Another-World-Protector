package de.z0rdak.yawp.api.core;

import de.z0rdak.yawp.core.region.GlobalRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.level.Level;

import java.util.Optional;
import java.util.Set;

public interface IRegionManager {

    GlobalRegion getGlobalRegion();

    void resetGlobal();

    Optional<IProtectedRegion> getDimensionalRegion(ResourceKey<Level> dim);

    Optional<DimensionRegionCache> getDimensionCache(ResourceKey<Level> dim);

    void save();

    /**
     * Gets the DimensionalRegion API for the specified dimension key.
     *
     * @param dim the dimension key to get the API for
     * @return the DimensionalRegionApi for the specified dimension key if it exists, otherwise Optional.Empty
     */
    Optional<IDimensionRegionApi> getDimRegionApi(ResourceKey<Level> dim);

    /**
     * Gets the DimensionalRegion API for the specified dimension key (E.g. "minecraft:overworld").
     *
     * @param dimKey the dimension key to get the API for
     * @return the DimensionalRegionApi for the specified dimension key if it exists, otherwise Optional.Empty
     */
    Optional<IDimensionRegionApi> getDimRegionApiByKey(String dimKey);


    ResourceKey<Level> getDimApiKey(String dimKey);

    /**
     * Check whether a DimensionalRegion for the specified dimension already exists.
     *
     * @param dim the dimension key to check for
     * @return true if a DimensionalRegion exists, false otherwise
     */
    boolean hasRegionFor(ResourceKey<Level> dim);

    /**
     * Creates a new DimensionalRegionCache (and DimensionalRegion) for the specified dimension.
     *
     * @param dim the dimension identifier of the dimension
     * @return true if a new DimensionalRegionCache was created, false if it already existed
     */
    boolean createDimRegion(ResourceKey<Level> dim);


    Set<ResourceKey<Level>> getDimensions();

    void resetDimension(ResourceKey<Level> dim);
}

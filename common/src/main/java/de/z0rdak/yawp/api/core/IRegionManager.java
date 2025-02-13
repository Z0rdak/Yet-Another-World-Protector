package de.z0rdak.yawp.api.core;

import de.z0rdak.yawp.core.region.GlobalRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.data.region.DimensionRegionCache;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.level.Level;

import java.util.Optional;
import java.util.Set;

public interface IRegionManager {

    GlobalRegion getGlobalRegion();

    /**
     * This only resets the Global Region, not its child regions.
     */
    void resetGlobal();

    /**
     * You can safely cast the returned instance of to DimensionalRegion, but it doesn't provide any benefit.
     * @param dim the resource key of the dimension/level
     *      * @return the DimensionRegionCache corresponding to dim
     */
    Optional<IProtectedRegion> getDimensionalRegion(ResourceKey<Level> dim);

    /**
     * Gets the DimensionRegionCache for the specified dimension. 
     * A DimensionRegionCache manages the DimensionalRegion and all Local Regions of the corresponding dimensions.
     * @param dim the resource key of the dimension/level
     * @return the DimensionRegionCache corresponding to dim
     */
    Optional<DimensionRegionCache> getDimensionCache(ResourceKey<Level> dim);

    /**
     * Flag the scheduler to save the region data. This usually happens either 
     * - cyclic (when enabled) 
     * - when leaving the world (in single player) 
     * - shutting down the server gracefully 
     * - or executing the '/save-all' command.
     */
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

    /**
     * Create the corresponding ResourceKey for the provided resource key string (e.g. 'minecraft:overworld')
     * Basically a wrapper around `ResourceKey.create(Registries.DIMENSION, ResourceLocation.parse(dimKey));`
     * @param dimKey resource key of the level/dimension
     * @return the corresponding ResourceKey for the level/dimension
     */
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

    /**
     * Returns a set of resource keys for all created Dimensional Regions
     * @return a set of resource keys corresponding to registered DimensionalRegions
     */
    Set<ResourceKey<Level>> getDimensions();

    /**
     * Resets the DimensionalRegion as well as all LocalRegions of the corresponding level.
     * @param dim the resource key of the level/dimension you want to reset its corresponding data for.
     */
    void resetDimension(ResourceKey<Level> dim);
}

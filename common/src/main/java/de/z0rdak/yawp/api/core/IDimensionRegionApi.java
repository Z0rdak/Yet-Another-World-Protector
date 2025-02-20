package de.z0rdak.yawp.api.core;

import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Vec3i;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.levelgen.structure.BoundingBox;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

public interface IDimensionRegionApi {

    void save();

    Optional<IMarkableRegion> getLocalRegion(String name);

    ResourceKey<Level> getDimKey();

    /**
     * Test if a name is available for creating a local region
     *
     * @param name the name of the new Local Region
     * @return true if the name is suitable for creating a new region and is not taken, false otherwise
     */
    boolean hasLocal(String name);

    /**
     * Check if any regions exists at the given position
     *
     * @param pos the position to check for regions
     * @return true if any regions exist at the given position, false otherwise
     */
    boolean hasRegionAt(BlockPos pos);

    /**
     * Get all regions at the given position
     *
     * @param pos the position to check for regions
     * @return a list of all regions at the given position, may be empty if no regions exist
     */
    List<IMarkableRegion> getRegionsAt(BlockPos pos);


    Collection<IMarkableRegion> getAllLocalRegions();

    /**
     * Get all regions at the given Cuboid area defined by two Vec3i positions
     *
     * @param pos1 the first position of the cuboid area
     * @param pos2 the second position of the cuboid area
     * @return a list of all regions in the given cuboid area, may be empty if no regions exist
     */
    List<IMarkableRegion> getRegionsIn(Vec3i pos1, Vec3i pos2);

    /**
     * Get all regions inside the given Cuboid area, defined by the coordinates
     *
     * @param x1 xpos of the first block
     * @param y1 ypos of the first block
     * @param z1 zpos of the first block
     * @param x2 xpos of the second block
     * @param y2 ypos of the second block
     * @param z2 zpos of the second block
     * @return a list of all regions in the given area, may be empty if no regions exist
     */
    List<IMarkableRegion> getRegionsInCoords(int x1, int y1, int z1, int x2, int y2, int z2);

    /**
     * Get all regions at the given AABB (AreaAlignedBlockBox - basically a Cuboid) area
     *
     * @param blockBox the AABB area to check for regions
     * @return a list of all regions in the given AABB area, may be empty if no regions exist
     */
    List<IMarkableRegion> getRegionsInBox(BoundingBox blockBox);

    List<IMarkableRegion> getIntersectingRegions(BoundingBox blockBox);

    /**
     * Returns all regions that intersect with the given region
     *
     * @param region the region to check for intersections
     * @return a list of all regions that intersect with the given region, may be empty if no regions intersect
     */
    List<IMarkableRegion> getIntersectingRegions(IMarkableRegion region);

    /**
     * Returns all regions that contain the given region
     *
     * @param region the region to check for containing regions
     * @return a list of all regions that contain the given region, may be empty if no regions contain the given region
     */
    List<IMarkableRegion> getContainingRegions(IMarkableRegion region);

    /**
     * Returns all regions that are contained by the given region
     *
     * @param region the region to check for contained regions
     * @return a list of all regions that are contained by the given region, may be empty if no regions are contained by the given region
     */
    List<IMarkableRegion> getContainedRegions(IMarkableRegion region);

    /**
     * Add a new Local Region providing the region itself
     *
     * @param region the region to be added to the dimension and saved
     * @return true if the Local Region was added successfully, false otherwise
     */
    boolean addLocalRegion(IMarkableRegion region);

    /**
     * Remove the provided region from the dimension
     *
     * @param region the region to be removed from the dimension and saved
     * @return true if the Local Region was removed successfully, false otherwise
     */
    boolean removeLocal(IMarkableRegion region);

    /**
     * Remove the region identified by the provided region name from the dimension
     *
     * @param regionName the region to be removed from the dimension and saved
     * @return true if the Local Region was removed successfully, false otherwise
     */
    boolean removeLocalRegion(String regionName);

    /**
     * Gets the region with the highest priority among all involved regions at the given location and dimension. <br>
     * This considers the active state of the region as well. <br>
     *
     * @param position the position to check for involved regions
     * @return the region with the highest priority among all involved regions which contain the given location
     */
    Optional<IMarkableRegion> getInvolvedRegionFor(BlockPos position);

    /**
     * Gets the responsible region for the given position and dimension. <br>
     * The responsible region is the region with the highest priority among all involved regions at the given location and dimension. <br>
     *
     * @param pos the position to get the responsible region for
     * @return the responsible region for the given position and dimension
     */
    Optional<IProtectedRegion> findResponsibleRegion(BlockPos pos);
}

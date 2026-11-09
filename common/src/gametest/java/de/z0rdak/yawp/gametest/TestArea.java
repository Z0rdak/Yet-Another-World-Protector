package de.z0rdak.yawp.gametest;

import de.z0rdak.yawp.api.core.ILevelRegionApi;
import de.z0rdak.yawp.api.core.RegionManager;
import de.z0rdak.yawp.api.core.region.CuboidRegionBuilder;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.CuboidRegion;
import de.z0rdak.yawp.core.region.MarkedRegion;
import net.minecraft.core.BlockPos;
import net.minecraft.gametest.framework.GameTestHelper;

/**
 * A helper class to create a temporary YAWP region around a GameTest area.
 * <p>
 *     {@code TestArea.around(helper).deny(RegionFlag.PLACE_BLOCKS).create()} creates a region around the test area that denies block placement.
 * </p>
 * <p>
 *     Callers should use the try-with-resources pattern to ensure the region is removed after the test.
 *     Except if it's a multi tick test, then the caller is responsible for calling close after the test.
 * </p>
 */
public class TestArea implements AutoCloseable {

    private final ILevelRegionApi api;
    private final MarkedRegion region;


    private TestArea(ILevelRegionApi api, MarkedRegion region) {
        this.api = api;
        this.region = region;
    }


    protected static TestArea around(GameTestHelper helper) {
        var level = helper.getLevel();
        var dim = level.dimension();
        RegionManager.get().trackLevel(dim);
        var api = RegionManager.get().getDimRegionApi(dim).orElseThrow(() -> new IllegalStateException("level is not tracked"));

        BlockPos min = helper.absolutePos(BlockPos.ZERO);
        BlockPos max = helper.absolutePos(new BlockPos(7, 7, 7));
        String name = "gametest_" + min.getX() + "_" + min.getY() + "_" + min.getZ();
        CuboidRegionBuilder builder = new CuboidRegionBuilder(name);
        builder.setArea(new CuboidArea(min, max));
        CuboidRegion region = builder.inDim(level).withDefaultPriority().on().build();
        return new TestArea(api, region);
    }

    public TestArea deny(RegionFlag... flags) {
        for (RegionFlag flag : flags) {
            this.region.addFlag(new BooleanFlag(flag, FlagState.DENIED, false));
        }
        return this;
    }

    /**
     * Creates the region in the level.
     * If a region with the same name already exists, it will be removed first.
     */
    public TestArea create() {
        if (this.region == null || this.region.getArea() == null || this.api == null) {
            throw new IllegalStateException("Region or API is not initialized");
        }
        api.removeLocalRegion(this.region.getName());
        api.addLocalRegion(this.region);
        return this;
    }

    /**
     * Removes the region from the level.
     * If the region does not exist, nothing happens.
     */
    @Override
    public void close() {
        api.removeLocalRegion(this.region.getName());
    }
}

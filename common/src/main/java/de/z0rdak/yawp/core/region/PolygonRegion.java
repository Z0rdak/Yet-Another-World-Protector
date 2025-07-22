package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.area.Polygon3DArea;
import de.z0rdak.yawp.core.area.RegionAnchors;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

public final class PolygonRegion extends MarkedRegion {

    public PolygonRegion(String name, Polygon3DArea area, Player owner, ResourceKey<Level> dimension) {
        super(name, area, owner, dimension);
    }

    public PolygonRegion(String name, Polygon3DArea area, RegionAnchors anchors, Player owner, ResourceKey<Level> dimension) {
        super(name, area, anchors, owner, dimension);
    }
}

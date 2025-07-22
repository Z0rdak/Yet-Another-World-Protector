package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.area.RegionAnchors;
import de.z0rdak.yawp.core.area.VerticalCylinderArea;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

public final class CylinderRegion extends MarkedRegion {

    public CylinderRegion(String name, VerticalCylinderArea area, RegionAnchors anchors, Player owner, ResourceKey<Level> dimension) {
        super(name, area, anchors, owner, dimension);
    }
}

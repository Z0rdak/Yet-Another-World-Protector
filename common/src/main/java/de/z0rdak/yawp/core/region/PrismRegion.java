package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.area.PrismArea;
import de.z0rdak.yawp.core.area.RegionAnchors;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

public final class PrismRegion extends MarkedRegion {

    public PrismRegion(String name, PrismArea area, Player owner, ResourceKey<Level> dimension) {
        super(name, area, owner, dimension);
    }

    public PrismRegion(String name, PrismArea area, RegionAnchors anchors, Player owner, ResourceKey<Level> dimension) {
        super(name, area, anchors, owner, dimension);
    }
}

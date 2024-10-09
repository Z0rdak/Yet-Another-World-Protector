package de.z0rdak.yawp.api.events.area;

import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.world.entity.player.Player;
import org.jetbrains.annotations.Nullable;

/**
 * Will eventually contain an event for entering and leaving the area
 */
public abstract class AreaEvent {

    private final IMarkableRegion region;
    private final IMarkableArea area;
    @Nullable
    private final Player player;

    private AreaEvent(final IMarkableRegion region, final IMarkableArea area, final @Nullable Player player) {
        this.area = area;
        this.player = player;
        this.region = region;
    }
    
    public IMarkableRegion getRegion() {
        return region;
    }
    
    public @Nullable Player getPlayer() {
        return player;
    }

    public IMarkableArea getArea() {
        return area;
    }
}
    



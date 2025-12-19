package de.z0rdak.yawp.platform.event;

import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.platform.services.event.RegionEventDispatcher;
import net.minecraftforge.common.MinecraftForge;

/**
 * Forge-specific backend for {@link RegionEventDispatcher}.
 * Integrates forge event bus with the common region event system.
 */
public final class ForgeRegionEventDispatcher implements RegionEventDispatcher {

    @Override
    public <T extends RegionEvent> boolean post(T event) {
        if (event instanceof RegionEvent.Create create) {
            ForgeRegionEvent.Create forgeEvent = new ForgeRegionEvent.Create(create);
            MinecraftForge.EVENT_BUS.post(forgeEvent);
            return forgeEvent.isCanceled() || create.isCanceled();
        }
        else if (event instanceof RegionEvent.Rename rename) {
            ForgeRegionEvent.Rename forgeEvent = new ForgeRegionEvent.Rename(rename);
            MinecraftForge.EVENT_BUS.post(forgeEvent);
            return forgeEvent.isCanceled() || rename.isCanceled();
        }
        else if (event instanceof RegionEvent.UpdateArea updateArea) {
            ForgeRegionEvent.UpdateArea forgeEvent = new ForgeRegionEvent.UpdateArea(updateArea);
            MinecraftForge.EVENT_BUS.post(forgeEvent);
            return forgeEvent.isCanceled() || updateArea.isCanceled();
        }
        else if (event instanceof RegionEvent.Remove remove) {
            ForgeRegionEvent.Remove forgeEvent = new ForgeRegionEvent.Remove(remove);
            MinecraftForge.EVENT_BUS.post(forgeEvent);
            return forgeEvent.isCanceled() || remove.isCanceled();
        }
        else if (event instanceof RegionEvent.PlayerEnter enter) {
            ForgeRegionEvent.ForgePlayerEnter forgeEvent = new ForgeRegionEvent.ForgePlayerEnter(
                    enter.getRegion(), enter.getPlayer(), enter.previous(), enter.current()
            );
            return MinecraftForge.EVENT_BUS.post(forgeEvent) || enter.isCanceled();
        }
        else if (event instanceof RegionEvent.PlayerLeave leave) {
            ForgeRegionEvent.ForgePlayerLeave forgeEvent = new ForgeRegionEvent.ForgePlayerLeave(
                    leave.getRegion(), leave.getPlayer(), leave.previous(), leave.current()
            );
            return MinecraftForge.EVENT_BUS.post(forgeEvent) || leave.isCanceled();
        }
        return false;
    }

}

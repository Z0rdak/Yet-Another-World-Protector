package de.z0rdak.yawp.api.events.region;

import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;

/**
 * Currently just a wrapper to reduce direct forge import from flag checks and make a future multi-loader-project easier.
 */
public final class RegionEvents {

    private RegionEvents() {
    }

    /**
     * Posts the provided event to the forge event bus
     */
    public static boolean post(FlagCheckEvent checkEvent) {
        return MinecraftForge.EVENT_BUS.post(checkEvent);
    }

    /**
     * Posts the provided event to the forge event bus
     */
    public static boolean post(Event event) {
        return MinecraftForge.EVENT_BUS.post(event);
    }

    /**
     * Posts the provided event to the mod event bus
     */
    public static boolean postToModBus(Event event) {
        return FMLJavaModLoadingContext.get().getModEventBus().post(event);
    }

    @FunctionalInterface
    public interface RenameRegion {
        boolean renameRegion(RegionEvent.RenameRegion renameRegionEvent);
    }

    @FunctionalInterface
    public interface UpdateArea {
        boolean updateArea(RegionEvent.UpdateArea updateAreaEvent);
    }

    @FunctionalInterface
    public interface FlagResult {
        FlagCheckResult getResult(FlagCheckResult flagCheckResult);
    }

    @FunctionalInterface
    public interface CheckFlag {
        boolean checkFlag(FlagCheckEvent flagCheckEvent);
    }

}

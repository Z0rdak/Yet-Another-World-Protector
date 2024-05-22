package de.z0rdak.yawp;

import de.z0rdak.yawp.config.ConfigRegistry;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.IExtensionPoint;
import net.neoforged.fml.ModLoadingContext;
import net.neoforged.fml.common.Mod;
import net.neoforged.fml.event.lifecycle.FMLCommonSetupEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Mod(YetAnotherWorldProtector.MODID)
public class YetAnotherWorldProtector {
    public static final String MODID = "yawp";
    public static final Logger LOGGER = LogManager.getLogger("YAWP");
    private static IEventBus yawpEventBus;

    public YetAnotherWorldProtector(IEventBus modEventBus) {
        yawpEventBus = modEventBus;
        modEventBus.addListener(this::onInit);
        //Make sure the mod being absent on the other network side does not cause the client to display the server as incompatible
        ModLoadingContext.get().registerExtensionPoint(IExtensionPoint.DisplayTest.class, () -> new IExtensionPoint.DisplayTest(() -> IExtensionPoint.DisplayTest.IGNORESERVERONLY, (s, b) -> true));
    }

    @SubscribeEvent
    public void onInit(FMLCommonSetupEvent event) {
        ConfigRegistry.register(yawpEventBus);
    }
}

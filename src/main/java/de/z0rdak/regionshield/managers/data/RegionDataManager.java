package de.z0rdak.regionshield.managers.data;

import de.z0rdak.regionshield.RegionShield;
import de.z0rdak.regionshield.util.constants.*;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.registry.Registry;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.world.World;
import net.minecraft.world.server.ServerWorld;
import net.minecraft.world.storage.DimensionSavedDataManager;
import net.minecraft.world.storage.WorldSavedData;
import net.minecraftforge.fml.event.server.FMLServerStartingEvent;
import net.minecraftforge.fml.server.ServerLifecycleHooks;

import java.util.*;
import java.util.stream.Collectors;

public class RegionDataManager extends WorldSavedData {

    private static final String DATA_NAME = RegionShield.MODID + "-regions";
    /**
     * Dimension -> { RegionName -> IMarkableRegion }
     */
    private static final Map<RegistryKey<World>, DimensionalRegionCache> regionMap = new HashMap<>();
    // Data instance
    private static RegionDataManager clientRegionCopy = new RegionDataManager();

    private RegionDataManager() {
        super(DATA_NAME);
    }

    public static void save(){
        RegionDataManager.get().setDirty();
    }

    public static RegionDataManager get() {
        if (clientRegionCopy == null) {
            MinecraftServer server = ServerLifecycleHooks.getCurrentServer();
            if (server != null) {
                ServerWorld overworld = server.overworld();
                if (!overworld.isClientSide) {
                    DimensionSavedDataManager storage = overworld.getDataStorage();
                    clientRegionCopy = storage.get(RegionDataManager::new, DATA_NAME);
                }

            }
        }
        return clientRegionCopy;
    }

    public static void onServerStarting(FMLServerStartingEvent event) {
        try {
            ServerWorld world = Objects.requireNonNull(event.getServer().overworld());
            if (!world.isClientSide) {
                DimensionSavedDataManager storage = world.getDataStorage();
                RegionDataManager data = storage.get(RegionDataManager::new, DATA_NAME);
                storage.set(data);
                clientRegionCopy = data;
                RegionShield.LOGGER.info(new TranslationTextComponent("console.logger.info.data.load.success", data.getAllRegionNames().size(), data.getDimensionList().size()).getString());
            }
        } catch (NullPointerException npe) {
            RegionShield.LOGGER.error(new TranslationTextComponent("console.logger.error.data.load.failure"));
        }
    }

    @Override
    public void load(CompoundNBT nbt) {
        clearRegions();
        CompoundNBT dimensionRegions = nbt.getCompound(DataNBT.REGIONS);
        for (String dimKey : dimensionRegions.getAllKeys()) {
            CompoundNBT dimRegionMap = dimensionRegions.getCompound(dimKey);
            DimensionalRegionCache dimCache = new DimensionalRegionCache(dimRegionMap);
            RegistryKey<World> dimension = RegistryKey.create(Registry.DIMENSION_REGISTRY, new ResourceLocation(dimKey));
            regionMap.put(dimension, dimCache);
        }
    }

    @Override
    public CompoundNBT save(CompoundNBT compound) {
        CompoundNBT dimRegionNbtData = new CompoundNBT();
        for (Map.Entry<RegistryKey<World>, DimensionalRegionCache> entry : regionMap.entrySet()) {
            String dim = entry.getKey().location().toString();
            CompoundNBT dimCompound = entry.getValue().serializeNBT();
            dimRegionNbtData.put(dim, dimCompound);
        }
        compound.put(DataNBT.REGIONS, dimRegionNbtData);
        return compound;
    }



    public Collection<String> getAllRegionNames() {
        return regionMap.values().stream()
                .flatMap(regionCache -> regionCache.getRegionNames().stream())
                .collect(Collectors.toList());
    }

    public Collection<String> getDimensionList() {
        return regionMap.keySet().stream()
                .map(entry -> entry.location().toString())
                .collect(Collectors.toList());
    }

    public void clearRegions() {
        regionMap.forEach((dim, cache) -> cache.clearRegions());
        setDirty();
    }

}

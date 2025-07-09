package de.z0rdak.yawp.external;

import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.data.region.DimensionRegionCache;
import de.z0rdak.yawp.data.region.RegionDataManager;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.level.Level;
import org.dynmap.DynmapCommonAPI;
import org.dynmap.DynmapCommonAPIListener;
import org.dynmap.markers.MarkerAPI;
import org.dynmap.markers.MarkerSet;

import java.util.Collection;
import java.util.Optional;

public class DynMapIntegration implements WebMapInitializer {

    private static final String MARKER_SET_ID = "yawp";

    @Override
    public void initialize(WebMapRegistry registry) {
        DynmapCommonAPIListener.register(new DynmapCommonAPIListener() {
            @Override
            public void apiEnabled(DynmapCommonAPI api) {
                Constants.LOGGER.info("Activate DynMap integration for YAWP");
                MarkerAPI markerAPI = api.getMarkerAPI();
                markerAPI.createMarkerSet(MARKER_SET_ID, "YAWP Regions", null, false);

                var listener = new DynMapIntegration.Listener(api);
                registry.register(listener);
            }
        });
    }

    private record Listener(DynmapCommonAPI dynmapCommonAPI) implements WebMapIntegration {

        @Override
        public void onLoad() {
            RegionDataManager.getDimensionCaches().stream()
                    .map(DimensionRegionCache::getAllLocal)
                    .flatMap(Collection::stream)
                    .forEach(region -> addMarker(region, region.getName()));
        }

        @Override
        public void on(RegionEvent.Create event) {
            var region = event.getRegion();
            addMarker(region, region.getName());
        }

        @Override
        public void on(RegionEvent.UpdateArea event) {
            var region = event.getRegion();
            removeMarker(region, region.getName());
            addMarker(region, region.getName());
        }

        @Override
        public void on(RegionEvent.Rename event) {
            IMarkableRegion region = event.getRegion();
            removeMarker(region, event.getOldName());
            addMarker(region, event.getNewName());
        }

        @Override
        public void on(RegionEvent.Remove event) {
            var region = event.getRegion();
            removeMarker(region, region.getName());
        }

        private void removeMarker(IMarkableRegion region, String name) {
            getMarkerSet()
                    .map(set -> set.findAreaMarker(name))
                    .ifPresent(m -> {
                        Constants.LOGGER.debug("YAWP-BlueMap remove marker for region {} in {}", name, region.getDim().location());
                    });
        }

        private void addMarker(IMarkableRegion region, String name) {
            var area = region.getArea();
            if (area instanceof CuboidArea cuboidArea) {
                var box = cuboidArea.getArea();
                var xCoordinates = new double[] { box.minX(), box.maxX() + 1 };
                var zCoordinates = new double[] { box.minZ(), box.maxZ() + 1 };
                var worldId = getWorldName(region.getDim());
                getMarkerSet().ifPresent(set -> {
                    Constants.LOGGER.debug("YAWP-BlueMap adding marker for region {} in {}", name, region.getDim().location());
                    var marker = set.createAreaMarker(name, name, true, worldId, xCoordinates, zCoordinates, false);
                    marker.setRangeY(box.minY(), box.maxY());
                    marker.setDescription(getDetails(region, name));
                });
            }
        }

        private static String getWorldName(ResourceKey<Level> key) {
            if (key == Level.OVERWORLD) {
                return "world";
            } else if (key == Level.END) {
                return "DIM1";
            } else if (key == Level.NETHER) {
                return "DIM-1";
            }
            return key.location().getNamespace() + "_" + key.location().getPath();
        }

        private Optional<MarkerSet> getMarkerSet() {
            var api = dynmapCommonAPI.getMarkerAPI();
            return Optional.of(api.getMarkerSet(MARKER_SET_ID));
        }
    }
}

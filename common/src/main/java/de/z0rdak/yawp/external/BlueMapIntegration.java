package de.z0rdak.yawp.external;

import de.bluecolored.bluemap.api.BlueMapAPI;
import de.bluecolored.bluemap.api.BlueMapMap;
import de.bluecolored.bluemap.api.BlueMapWorld;
import de.bluecolored.bluemap.api.markers.ExtrudeMarker;
import de.bluecolored.bluemap.api.markers.ExtrudeMarker.Builder;
import de.bluecolored.bluemap.api.markers.MarkerSet;
import de.bluecolored.bluemap.api.math.Shape;
import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.api.permission.Permissions;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.data.region.DimensionRegionCache;
import de.z0rdak.yawp.data.region.RegionDataManager;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.level.Level;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

public class BlueMapIntegration implements WebMapInitializer {

    private static final String MARKER_SET = "yawp";

    public BlueMapIntegration() {

    }

    @Override
    public void initialize(WebMapRegistry registry) {
        BlueMapAPI.onEnable(api -> {
            Constants.LOGGER.info("Activate BlueMap integration for YAWP");
            var listener = new Listener(api);
            listener.registerMarkerSetForAllMapsInAllDimensions();
            listener.createMarkerForRegions(RegionDataManager.getDimensionCaches());
            registry.register(listener);
        });
    }

    private record Listener(BlueMapAPI blueMapAPI) implements WebMapIntegration {
        void registerMarkerSetForAllMapsInAllDimensions() {
            blueMapAPI.getWorlds()
                    .stream()
                    .map(BlueMapWorld::getMaps)
                    .flatMap(Collection::stream)
                    .map(BlueMapMap::getMarkerSets)
                    .forEach(this::addMarkerSet);
        }

        private void addMarkerSet(Map<String, MarkerSet> markerSets) {
            var markerSet = MarkerSet.builder().label("YAWP Regions").build();
            markerSets.put(MARKER_SET, markerSet);
        }

        public void createMarkerForRegions(List<DimensionRegionCache> dimensionCaches) {
            dimensionCaches.forEach(cache -> {
                var marketSets = getMarkerSets(cache.dimensionKey());
                cache.getAllLocal().forEach(region -> marketSets.forEach(set -> addMarker(set, region, region.getName())));
            });
        }

        @Override
        public void on(RegionEvent.Create event) {
            var region = event.getRegion();
            getMarkerSets(region.getDim()).forEach(markerSet -> addMarker(markerSet, region, region.getName()));
        }

        @Override
        public void on(RegionEvent.Remove event) {
            var region = event.getRegion();
            getMarkerSets(region.getDim()).forEach(markerSet -> markerSet.remove(region.getName()));
        }

        @Override
        public void on(RegionEvent.Rename event) {
            recreateMarker(event.getOldName(), event.getNewName(), event.getRegion());
        }

        @Override
        public void on(RegionEvent.UpdateArea event) {
            var region = event.getRegion();
            var name = region.getName();
            recreateMarker(name, name, region);
        }

        private void recreateMarker(String oldName, String newName, IMarkableRegion region) {
            getMarkerSets(region.getDim()).forEach(markerSet -> {
                markerSet.remove(oldName);
                addMarker(markerSet, region, newName);
            });
        }

        private List<MarkerSet> getMarkerSets(ResourceKey<Level> dim) {
            return blueMapAPI.getWorld(dim)
                    .map(BlueMapWorld::getMaps)
                    .orElseGet(List::of)
                    .stream()
                    .map(BlueMapMap::getMarkerSets)
                    .map(sets -> sets.get(MARKER_SET))
                    .collect(Collectors.toList());
        }

        private void addMarker(MarkerSet markerSet, IMarkableRegion region, String name) {
            Optional.ofNullable(addShape(ExtrudeMarker.builder(), region.getArea())).
                    ifPresent(builder -> {
                        Constants.LOGGER.debug("YAWP-BlueMap adding marker for region {} in {}", name, region.getDim().location());
                        var marker = builder.label(name).detail(getDetails(region, name)).build();
                        markerSet.put(name, marker);
                    });
        }

        private Builder addShape(Builder marker, IMarkableArea area) {
            if (area instanceof CuboidArea cuboidArea) {
                var box = cuboidArea.getArea();
                var shape = Shape.createRect(box.minX(), box.minZ(), box.maxX() + 1, box.maxZ() + 1);
                return marker.shape(shape, box.minY(), box.maxY());
            }
            return null;
        }

        private String getDetails(IMarkableRegion region, String name) {
            var owners = region.getGroup(Permissions.OWNER);
            var ownerNames = owners.getPlayers().values().stream()
                    .map("<b> %s </b>"::formatted)
                    .collect(Collectors.joining(", "));
            return "%s owned by %s".formatted(name, ownerNames);
        }
    }
}

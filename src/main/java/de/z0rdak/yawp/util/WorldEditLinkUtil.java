package de.z0rdak.yawp.util;

import static de.z0rdak.yawp.util.MessageUtil.sendCmdFeedback;

import org.apache.commons.lang3.NotImplementedException;

import com.sk89q.worldedit.IncompleteRegionException;
import com.sk89q.worldedit.LocalSession;
import com.sk89q.worldedit.WorldEdit;
import com.sk89q.worldedit.fabric.FabricAdapter;
import com.sk89q.worldedit.math.BlockVector3;
import com.sk89q.worldedit.regions.CuboidRegion;
import com.sk89q.worldedit.regions.Region;
import com.sk89q.worldedit.regions.selector.CuboidRegionSelector;
import com.sk89q.worldedit.world.World;

import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.util.math.BlockPos;

public class WorldEditLinkUtil {

	private WorldEditLinkUtil() {
	}



	public static int regionToWorldEdit(ServerCommandSource src, IMarkableRegion region) {

		com.sk89q.worldedit.entity.Player actor = com.sk89q.worldedit.fabric.FabricAdapter.adaptPlayer(src.getPlayer());
		LocalSession sess = WorldEdit.getInstance().getSessionManager().getIfPresent(actor);
		if (sess==null) {
			return 0;
		}
		World world = actor.getWorld();
		if (world==null) {
			return 0;
		}
		IMarkableArea area = region.getArea();
		switch (area.getAreaType()) {
		case CUBOID: {
			CuboidArea cuboidArea = (CuboidArea) area;
			BlockVector3 pos1 = FabricAdapter.adapt(cuboidArea.getAreaP1());
			BlockVector3 pos2 = FabricAdapter.adapt(cuboidArea.getAreaP2());
			CuboidRegionSelector we_reg_sel;
			if (true) {
			    // Live linking
				we_reg_sel = new WorldEditLinkCuboidRegionSelector(src, region, world, pos1, pos2);
			} else {
				// Single shot
			    we_reg_sel = new CuboidRegionSelector(world, pos1, pos2);
			}
			sess.setRegionSelector(world, we_reg_sel);
			we_reg_sel.explainRegionAdjust(actor, sess);
			return 0;
		}
		case CYLINDER:
			throw new NotImplementedException("cylinder");
		case SPHERE:
			throw new NotImplementedException("sphere");
		case POLYGON_3D:
			throw new NotImplementedException("polygon");
		case PRISM:
			throw new NotImplementedException("prism");
		default:
			throw new IllegalArgumentException("Invalid area type");
		}
	}

	public static int worldEditToRegion(ServerCommandSource src, IMarkableRegion region) {

		com.sk89q.worldedit.entity.Player actor = com.sk89q.worldedit.fabric.FabricAdapter.adaptPlayer(src.getPlayer());
		LocalSession sess = WorldEdit.getInstance().getSessionManager().getIfPresent(actor);
		if (sess==null) {
			// Silently ignore calls from missing players
			return 0;
		}
		World world = actor.getWorld();
		if (world==null) {
			// FIXME - add error message
			return 0;
		}
		Region we_region;
		try {
			we_region = sess.getSelection(world);
		} catch (IncompleteRegionException e) {
			// FIXME - add error message
			return 0;
		}
		if (we_region instanceof CuboidRegion) {
			// Feed WorldEdit selection to YAWP
			CuboidRegion we_cuboid = (CuboidRegion) we_region;
			BlockPos pos1 = FabricAdapter.toBlockPos(we_cuboid.getPos1());
			BlockPos pos2 = FabricAdapter.toBlockPos(we_cuboid.getPos2());
			region.setArea(new CuboidArea(pos1, pos2));
		} else {
			// FIXME - add error message
			return 0;
		}
		return 0;
	}

}

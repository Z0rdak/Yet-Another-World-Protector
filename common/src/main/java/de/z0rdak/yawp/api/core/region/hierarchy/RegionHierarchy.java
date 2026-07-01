package de.z0rdak.yawp.api.core.region.hierarchy;

import de.z0rdak.yawp.api.core.RegionManager;
import de.z0rdak.yawp.core.region.*;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

public final class RegionHierarchy {

    /**
     * Validates whether a parent-child relationship is allowed
     * under all hierarchy constraints.
     *
     * Checks include:
     * - cycle prevention
     * - type compatibility
     * - dimension consistency
     * - containment rules (for markable regions)
     * - priority constraints
     */
    public static HierarchyValidationResult validateParent(IProtectedRegion child, IProtectedRegion parent) {
        if (child == null || parent == null) {
            return HierarchyValidationResult.invalidParentType();
        }
        // Global is immutable root (self-parent only)
        if (child instanceof GlobalRegion) {
            if (parent != child) {
                return HierarchyValidationResult.invalidParentType();
            }
            return HierarchyValidationResult.validResult();
        }
        // No region may attach to null (strict invariant)
        if (parent == null) {
            return HierarchyValidationResult.invalidParentType();
        }
        // cycle prevention (full chain check)
        if (wouldCreateCycle(child, parent)) {
            return HierarchyValidationResult.cycleDetected();
        }
        if (!isCompatibleType(child, parent)) {
            return HierarchyValidationResult.invalidParentType();
        }
        if (!isSameDimension(child, parent)) {
            return HierarchyValidationResult.differentDimension();
        }
        // containment rule for markable regions
        if (parent instanceof IMarkableRegion markedParent && child instanceof IMarkableRegion markedChild) {
            if (!contains(markedParent, markedChild)) {
                return HierarchyValidationResult.containmentFailed();
            }
        }
        //if (child instanceof IMarkableRegion markedChild) {
        //    if (!hasValidPriority(markedChild, parent)) {
        //        return HierarchyValidationResult.invalidPriority();
        //    }
        //}
        return HierarchyValidationResult.validResult();
    }

    /**
     * Recursively normalizes the priorities of an entire region subtree.
     *
     * Starting at the given region, each markable region is normalized relative
     * to its nearest markable parent, ensuring that every child has a strictly
     * higher priority than its closest markable ancestor.
     *
     * This method is primarily intended as a repair pass after reconstructing a
     * region hierarchy, such as after deserialization or loading region data
     * from persistent storage. It may also be used after bulk hierarchy
     * modifications to restore priority consistency.
     *
     * For normal parent changes, {@link #setParent(IProtectedRegion, IProtectedRegion)}
     * already normalizes the moved region. This method should only be used when
     * an entire subtree may require revalidation.
     */
    public static void normalizePriorityTree(IMarkableRegion region) {
        normalizePriority(region);

        for (IProtectedRegion child : region.getChildren().values()) {
            if (child instanceof IMarkableRegion markedChild) {
                normalizePriorityTree(markedChild);
            }
        }
    }

    /**
     * Normalizes the priorities of all local regions belonging to the given
     * dimension.
     *
     * Unlike {@link #normalizePriorityTree(IMarkableRegion)}, this method does
     * not rely on the dimensional region's child hierarchy. Instead, it visits
     * every local region registered for the dimension and recursively
     * normalizes the priority of each region subtree.
     *
     * This method is intended to repair priority invariants after loading a
     * dimension from persistent storage, where not every local region is
     * necessarily a direct child of the dimensional region.
     */
    public static void normalizeDimensionPriorities(DimensionalRegion dimension) {

        RegionManager.get()
                .getLevelRegionData(dimension.getDim())
                .get()
                .getLocalList()
                .stream()
                .filter(r -> r.getParent() == dimension)
                .forEach(RegionHierarchy::normalizePriorityTree);
    }

    /**
     * Removes a region from its current parent and re-attaches it
     * to its fallback parent (never null).
     *
     * This preserves the invariant that all regions remain anchored
     * in the hierarchy tree.
     *
     * Fallback rules:
     * - MarkedRegion → containing Dimension
     * - DimensionalRegion → Global
     * - GlobalRegion → self-root
     */
    public static void removeParent(IProtectedRegion child) {
        IProtectedRegion fallback = resolveFallbackParent(child);
        if (fallback == null) {
           throw new IllegalStateException("Broken hierarchy.");
        }
        setParent(child, fallback);

        if (child instanceof IMarkableRegion marked) {
            normalizePriority(marked);
            fixDownward(marked);
        }
    }

    /**
     * Determines the correct fallback parent for a region when its
     * current parent is removed.
     *
     * Ensures the hierarchy invariant that no region is ever orphaned.
     */
    public static IProtectedRegion resolveFallbackParent(IProtectedRegion region) {
        if (region instanceof MarkedRegion marked) {
            IProtectedRegion dim = RegionManager.get().getDimensionalRegion(marked.getDim()).orElseThrow();
            return dim != null ? dim : RegionManager.get().getGlobalRegion();
        }
        if (region instanceof DimensionalRegion) {
            return  RegionManager.get().getGlobalRegion();
        }
        if (region instanceof GlobalRegion global) {
            return global;
        }
        return RegionManager.get().getGlobalRegion();
    }

    /**
     * Repairs a MarkedRegion whose parent is no longer valid.
     *
     * Ensures it is always attached to its containing dimension.
     */
    public static void repairMarkedRegion(IMarkableRegion region) {
        IProtectedRegion parent = region.getParent();
        if (parent instanceof IMarkableRegion) {
            return;
        }
        IProtectedRegion dim = RegionManager.get().getDimensionalRegion(region.getDim()).orElseThrow();
        if (dim != null) {
            setParent(region, dim);
        }
    }

    /**
     * Enforces structural invariants for special region types
     * (Global, Dimensional).
     *
     * Intended for use during load and repair operations only.
     */
    public static void enforceDimensionalInvariant(DimensionalRegion region) {
        if (!(region.getParent() instanceof GlobalRegion)) {
            setParent(region,  RegionManager.get().getGlobalRegion());
        }
    }

    /**
     * Enforces structural invariants for special region types
     * (Global, Dimensional).
     *
     * Intended for use during load and repair operations only.
     */
    public static void enforceGlobalInvariant(GlobalRegion global) {
        global.setParent(global);
    }

    /**
     * Enforces structural invariants for special region types
     * (Global, Dimensional).
     *
     * Intended for use during load and repair operations only.
     */
    public static void enforceStructuralInvariants(IProtectedRegion region) {
        if (region instanceof DimensionalRegion dim) {
            setParent(dim, RegionManager.get().getGlobalRegion());
        }

        if (region instanceof GlobalRegion global) {
            global.setParent(global);
        }
    }

    /**
     * Attaches a child region to a new parent and updates both sides of the
     * hierarchy relationship.
     *
     * This is the only supported mutation entry point for re-parenting regions.
     * All hierarchy changes should go through this method.
     *
     * Preconditions:
     * - {@link #validateParent(IProtectedRegion, IProtectedRegion)} has returned
     *   a valid result.
     *
     * Guarantees:
     * - the child is detached from its previous parent
     * - the child is attached to the new parent
     * - both parent and child references are kept consistent
     * - markable region priorities are normalized to satisfy the hierarchy
     *   invariant (a child must have a higher priority than its nearest
     *   markable ancestor)
     *
     * Note:
     * This method updates only the moved region. If an operation may invalidate
     * the priorities of descendants (e.g. moving an entire subtree), invoke
     * {@link #normalizePriorityTree(IMarkableRegion)} afterwards.
     */
    public static void setParent(IProtectedRegion child, IProtectedRegion parent) {
        IProtectedRegion oldParent = child.getParent();
        if (oldParent != null) {
            oldParent.removeChild(child);
        }
        // TODO check cycle
        parent.addChild(child);
        child.setParent(parent);
        if (child instanceof IMarkableRegion marked) {
            normalizePriority(marked);
        }
    }

    /**
     * Attaches a region to a new parent.
     *
     * Performs validation, updates both hierarchy links and
     * restores priority invariants.
     */
    public static HierarchyValidationResult attach(IProtectedRegion child, IProtectedRegion parent) {
        HierarchyValidationResult result = RegionHierarchy.validateParent(child, parent);
        if (!result.valid()) {
            return result;
        }
        RegionHierarchy.setParent(child, parent);
        if (child instanceof IMarkableRegion marked) {
            RegionHierarchy.normalizePriorityTree(marked);
        }
        return HierarchyValidationResult.validResult();
    }

    public static HierarchyValidationResult detach(IProtectedRegion child) {
        IProtectedRegion fallback = RegionHierarchy.resolveFallbackParent(child);
        return attach(child, fallback);
    }

    public static HierarchyValidationResult attachChild(IProtectedRegion parent, IProtectedRegion child) {
        return attach(child, parent);
    }

    public static HierarchyValidationResult detachChild(IProtectedRegion parent, IProtectedRegion child) {
        if (child.getParent() != parent) {
            return HierarchyValidationResult.invalidParentType();
        }
        return detach(child);
    }

    /**
     * Ensures that a markable region satisfies the hierarchy priority invariant
     * relative to its nearest markable parent.
     *
     * If the region's priority is less than or equal to its parent's priority,
     * it is increased to {@code parent.priority + 1}.
     *
     * This method only normalizes the given region and does not inspect or
     * modify any descendants. Use {@link #normalizePriorityTree(IMarkableRegion)}
     * after structural changes that may invalidate the priorities of an entire
     * subtree.
     */
    public static void normalizePriority(IMarkableRegion region) {
        var parent = region.getParent();
        if (parent instanceof IMarkableRegion markedParent) {
            int required = markedParent.getPriority() + 1;
            if (region.getPriority() < required) {
                region.setPriority(required);
            }
        }
    }

    public static void fixUpward(IMarkableRegion region) {

        IProtectedRegion parent = region.getParent();

        while (parent != null) {

            if (parent instanceof IMarkableRegion markedParent) {

                if (region.getPriority() <= markedParent.getPriority()) {
                    region.setPriority(markedParent.getPriority() + 1);
                }

                region = markedParent; // move upward
            }

            parent = parent.getParent();
        }
    }

    public static void fixDownward(IMarkableRegion region) {

        Deque<IMarkableRegion> stack = new ArrayDeque<>();
        stack.push(region);

        while (!stack.isEmpty()) {

            IMarkableRegion current = stack.pop();

            int basePriority = current.getPriority();

            for (IProtectedRegion child : current.getChildren().values()) {

                if (child instanceof IMarkableRegion markedChild) {

                    if (markedChild.getPriority() <= basePriority) {
                        markedChild.setPriority(basePriority + 1);
                    }

                    stack.push(markedChild);
                }
            }
        }
    }

    private static boolean wouldCreateCycle(IProtectedRegion child, IProtectedRegion parent)  {
        IProtectedRegion current = parent;
        while (current != null) {
            if (current == child) {
                return true;
            }
            if (current == current.getParent()) {
                break; // reached Global
            }
            current = current.getParent();
        }
        return false;
    }

    private static boolean isCompatibleType(
            IProtectedRegion child,
            IProtectedRegion parent
    ) {
        if (parent instanceof GlobalRegion) {
            return true;
        }

        if (parent instanceof DimensionalRegion) {
            return !(child instanceof GlobalRegion);
        }

        return true;
    }

    private static boolean isSameDimension(
            IProtectedRegion child,
            IProtectedRegion parent
    ) {
        if (child instanceof DimensionalRegion c
                && parent instanceof DimensionalRegion p) {

            return c.getDim().identifier().equals(p.getDim().identifier());
        }

        return true;
    }

    private static boolean hasValidPriority(IMarkableRegion region) {
        IProtectedRegion parent = region.getParent();
        while (parent != null) {
            if (parent instanceof IMarkableRegion markedParent) {
                if (region.getPriority() <= markedParent.getPriority()) {
                    return false;
                }
                break;
            }
            parent = parent.getParent();
        }
        return true;
    }

    private static boolean hasValidPriority(
            IMarkableRegion child,
            IProtectedRegion parent
    ) {
        if (!(parent instanceof IMarkableRegion markedParent)) {
            // Dimensional parent is always valid.
            return true;
        }

        return child.getPriority() > markedParent.getPriority();
    }

    public static List<IProtectedRegion> pathToRoot(IProtectedRegion region) {
        List<IProtectedRegion> path = new ArrayList<>();
        IProtectedRegion current = region;
        while (current != null) {
            path.add(current);
            if (current instanceof GlobalRegion)
                break;
            current = current.getParent();
        }
        return path;
    }

    public static List<IProtectedRegion> ancestorsOf(IProtectedRegion region) {
        List<IProtectedRegion> ancestors = new ArrayList<>();
        IProtectedRegion current = region.getParent();
        while (current != null && !(current instanceof GlobalRegion)) {
            ancestors.add(current);
            current = current.getParent();
        }
        return ancestors;
    }

    public static boolean isAncestor(IProtectedRegion ancestor, IProtectedRegion region) {
        IProtectedRegion current = region.getParent();
        while (current != null) {
            if (current.equals(ancestor)) {
                return true;
            }
            current = current.getParent();
        }
        return false;
    }

    public static boolean isDescendant(IProtectedRegion descendant, IProtectedRegion region) {
        return isAncestor(region, descendant);
    }

    public static boolean isRoot(IProtectedRegion region) {
        return region.getParent() instanceof GlobalRegion;
    }

    public static boolean contains(IMarkableRegion parent, IMarkableRegion child) {
        return parent.getArea().containsOther(child.getArea());
    }
}